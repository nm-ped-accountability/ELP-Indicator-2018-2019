
# R Script for the ELP Indicator



# Load Data ---------------------------------------------------------------

library(tidyverse)

rm(list = ls())

year0_raw <- read.csv("NM_Summative_Stud_file 2017.csv", 
                  header = TRUE, stringsAsFactors = FALSE)

year1_raw <- read.csv("ACCESS for ELLs 2017-2018 Complete Cases 2019-08-27.csv",
                  header = TRUE, stringsAsFactors = FALSE)

year2_raw <- read.csv("ACCESS for ELLs 2018-2019 Complete Cases 2019-07-18.csv",
                  header = TRUE, stringsAsFactors = FALSE)

target <- read.csv("ELP targets 2019.csv", 
                   header = TRUE, stringsAsFactors = FALSE)

schools <- read.csv("Master Schools 2019 V4.csv",
                    header = TRUE, stringsAsFactors = FALSE)

names(schools) <- tolower(names(schools))

schools18 <- schools[schools$ï..sy == 2018, ]
nrow(schools18) # N = 1788

schools19 <- schools[schools$ï..sy == 2019, ]
nrow(schools19) # N = 1802



# Data Cleaning and Preparation -------------------------------------------

## Tidy Year0 =============================================================

year0 <- year0_raw %>%
    select(District.Number, School.Number, State.Student.ID, 
           Grade, Composite..Overall..Proficiency.Level) %>%
    
    # remove students who do not have SSIDs
    filter(!is.na(State.Student.ID)) %>%
    
    # remove students who do not have composite scores
    filter(!is.na(Composite..Overall..Proficiency.Level)) %>%
    
    # formatting
    mutate(distcode = as.numeric(gsub("NM", "", District.Number)),
           schcode = School.Number,
           schnumb = distcode * 1000 + schcode,
           stid = State.Student.ID,
           grade = Grade,
           pl = Composite..Overall..Proficiency.Level,
           sy = 2017) %>%
    select(distcode, schcode, schnumb, stid, grade, pl, sy) %>%
    
    # remove BIE schools
    filter(distcode < 600 & !is.na(pl))

head(year0)

# check IDs
range(year0$stid) # valid
year0[duplicated(year0$stid), ] # no duplicates

# check grades
table(year0$grade) # 0-12


## Tidy Year1 =============================================================
# this data file has already be tidied (no BIE, no missing composite PLs)
year1 <- year1_raw %>%
    select(test_schnumb, distcode, schcode, stid, grade, PL_composite) %>%
    mutate(schnumb = test_schnumb,
           pl = PL_composite,
           sy = 2018) %>%
    select(distcode, schcode, schnumb, stid, grade, pl, sy) %>%
    filter(distcode < 600 & !is.na(pl))

head(year1)

# check IDs
range(year1$stid) # valid
year1[duplicated(year1$stid), ] # no duplicates

# check grades
table(year1$grade) # 0-12


## Tidy Year2 =============================================================
# this data file has already be tidied (no BIE, no missing composite PLs)
year2 <- year2_raw %>%
    select(test_schnumb, distcode, schcode, stid, STARS_grade, PL_composite) %>%
    mutate(schnumb = test_schnumb,
           grade = STARS_grade,
           pl = PL_composite,
           sy = 2019) %>%
    select(distcode, schcode, schnumb, stid, grade, pl, sy) %>%
    filter(distcode < 600 & !is.na(pl))

head(year2)

# check IDs
range(year2$stid) # valid
year2[duplicated(year2$stid), ] # no duplicates

# check grades
table(year2$grade) # 0-12



# Merge Multi-Year Data Files ---------------------------------------------

## One-Year Growth ========================================================

# merge year0 and year1; then add target scores for SY 2016-2017
year0100 <- left_join(year1, year0, by = "stid") %>%
    select(distcode.x, schcode.x, schnumb.x, stid, grade.x, pl.x, grade.y, pl.y) %>%
    rename(distcode = distcode.x,
           schcode = schcode.x,
           schnumb = schnumb.x,
           grade01 = grade.x,
           pl01 = pl.x,
           grade00 = grade.y,
           pl00 = pl.y) %>%
    
    # remove students who did not have valid current scores
    filter(!is.na(pl01)) %>%
    
    # remove students who did not have valid prior scores
    filter(!is.na(pl00)) %>%
    
    # remove students who were 12th graders at the time of identification
    filter(!(grade00 == 12)) %>%
    
    # add targets based on the entry year's scores and grades
    left_join(target, by = c("grade00" = "grade", "pl00" = "year00")) %>%
    select(-c(year02, year03, year04, year05)) %>%
    mutate(diff = pl01 - year01,
           met = diff >= 0,
           statecode = 0)

head(year0100)
nrow(year0100) # N = 35686

# save outputs
current_date <- Sys.Date()
file_name <- paste0("Student 1-Year ELP Growth (2017-18) ", current_date, ".csv")
write.csv(year0100, file = file_name, row.names = FALSE, na = "")


## Two-Year Growth ========================================================

# merge year0, year1, and year2; then add target scores for SY2018-2019
year020100 <- 
    # merge year2 and year1
    left_join(year2, year1, by = "stid") %>%
    select(distcode.x, schcode.x, schnumb.x, stid, grade.x, pl.x, grade.y, pl.y) %>%
    rename(distcode = distcode.x,
           schcode = schcode.x,
           schnumb = schnumb.x,
           grade02 = grade.x,
           pl02 = pl.x,
           grade01 = grade.y,
           pl01 = pl.y) %>%
    
    # add prior2 scores and grades
    left_join(year0, by = "stid") %>%
    select(-c(distcode.y, schcode.y, schnumb.y, sy)) %>%
    rename(distcode = distcode.x,
           schcode = schcode.x,
           schnumb = schnumb.x,
           grade00 = grade,
           pl00 = pl) %>%
    
    # add targets based on the entry year's scores and grades
    left_join(target, c("grade00" = "grade", "pl00" = "year00")) %>%
    select(-c(year03, year04, year05)) %>%
    rename(year01_entry_year = year01,
           year02_entry_year = year02) %>%
    
    # add targets based on the 1st year's scores and grades
    left_join(target, c("grade01" = "grade", "pl01" = "year00")) %>%
    select(-c(year02, year03, year04, year05)) %>% 
    
    # remove students who do not have valid current scores
    filter(!is.na(pl02)) %>%
    
    # remove students who do not have any prior scores
    filter(!(is.na(pl00) & is.na(pl01))) %>%
    
    # remove students who were 12th graders at the time of first valid score
    filter(is.na(grade00) | grade00 < 12) %>%
    filter(is.na(grade01) | !(is.na(grade00) & grade01 == 12)) %>%

    # select targets based on when each student entered EL status
    mutate(target = ifelse(is.na(grade00), year01,
                           ifelse(!is.na(grade00), year02_entry_year, "no")),
           diff = pl02 - as.numeric(target),
           met = diff >= 0,
           statecode = 0)


head(year020100)
nrow(year020100) # N = 41008

# save outputs
current_date <- Sys.Date()
file_name <- paste0("Student 2-Year ELP Growth (2017-19) ", current_date, ".csv")
write.csv(year020100, file = file_name, row.names = FALSE, na = "")



# Functions for Calculating Rates and Points ------------------------------

## Function for Calculating Percent Met ===================================
ELP_indicator <- function(dataset, code) {
    dataset %>%
        select(code, distcode, schcode, stid, diff, met) %>%
        group_by(dataset[[code]]) %>%
        filter(!is.na(diff)) %>%
        summarize(percent_met = mean(met),
                  mean_diff = mean(diff),
                  n_students = n())
}


## Function for Calculating Points ========================================

# school_level
school_level <- function(dataset) {
    dat <- ELP_indicator(dataset, "schnumb") %>%
        rename(schnumb = `dataset[[code]]`) %>%
        left_join(schools18, by = "schnumb") %>%
        select(schnumb, agaid, distcode, distname, schcode, schname, hs, 
               percent_met, mean_diff, n_students) %>%
        mutate(total_points = ifelse(hs == "Y", 5, 
                                     ifelse(hs == "N", 10, "no")),
               points = as.numeric(total_points) * percent_met,
               percent_met = percent_met * 100)    
}

# district_level
district_level <- function(dataset) {
    dat <- ELP_indicator(dataset, "distcode") %>%
        rename(distcode = `dataset[[code]]`) %>%
        left_join(schools18, by = "distcode") %>%
        select(distcode, distname, percent_met, mean_diff, n_students) %>%
        mutate(schnumb = distcode * 1000,
               agaid = NA,
               schcode = 0,
               schname = "Districtwide",
               hs = NA,
               percent_met = percent_met * 100,
               total_points = NA,
               points = NA)
        dat <- dat[!duplicated(dat$distcode), ]
}

# state_level
state_level <- function(dataset) {
    dat <- ELP_indicator(dataset, "statecode") %>%
        rename(distcode = `dataset[[code]]`) %>%
        mutate(schcode = 0,
               schnumb = 0,
               agaid = NA,
               distname = "Statewide",
               schname = "Statewide",
               hs = NA,
               percent_met = percent_met * 100,
               total_points = NA,
               points = NA)    
}



# Run Functions -----------------------------------------------------------

## One-Year Growth (2017-2018) ============================================
# run functions and sort columns in the same order in each output
school_ELP_1 <- school_level(year0100) %>% 
    select(sort(names(.)))

district_ELP_1 <- district_level(year0100) %>% 
    select(sort(names(.)))

state_ELP_1 <- state_level(year0100) %>% 
    select(sort(names(.)))

# merge files
final_1 <- rbind(school_ELP_1, district_ELP_1, state_ELP_1)
head(final_1)

# save output
current_date <- Sys.Date()
file_name <- paste0("ELP Indicator Points 2017-18 ", current_date, ".csv")
write.csv(final_1, file = file_name, row.names = FALSE, na = "")


## Two-Year Growth (2017-2019) =========================================
# run functions and sort columns in the same order in each output
school_ELP_2 <- school_level(year020100) %>% 
    select(sort(names(.)))

district_ELP_2 <- district_level(year020100) %>% 
    select(sort(names(.)))

state_ELP_2 <- state_level(year020100) %>% 
    select(sort(names(.)))

# merge files
final_2 <- rbind(school_ELP_2, district_ELP_2, state_ELP_2)
head(final_2)

# save outputs
current_date <- Sys.Date()
file_name <- paste0("ELP Indicator Points 2017-19 ", current_date, ".csv")
write.csv(final_2, file = file_name, row.names = FALSE, na = "")

