### ELP Indicator 201-2018


# Load data ---------------------------------------------------------------

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

schools18 <- schools[schools$ï..SY == 2018, ]


# Match year0 and year1 ---------------------------------------------------

# SY 2016-2017

# clean up year0
year0 <- year0_raw %>%
    select(District.Number, School.Number, State.Student.ID, 
           Grade, Composite..Overall..Proficiency.Level) %>%
    filter(!is.na(Composite..Overall..Proficiency.Level)) %>%
    mutate(distcode = as.numeric(gsub("NM", "", District.Number)),
           schcode = School.Number,
           schnumb = distcode * 1000 + schcode,
           stid = State.Student.ID,
           grade = Grade,
           pl = Composite..Overall..Proficiency.Level,
           sy = 2017) %>%
    select(distcode, schcode, schnumb, stid, grade, pl, sy)

head(year0)
year0[duplicated(year0$stid), ]

# clean up year1
year1 <- year1_raw %>%
    select(test_schnumb, distcode, schcode, stid, grade, PL_composite) %>%
    mutate(schnumb = test_schnumb,
           pl = PL_composite,
           sy = 2018) %>%
    select(distcode, schcode, schnumb, stid, grade, pl, sy)

head(year1)

# merge year0 and year1 and add target scores
year0100 <- left_join(year1, year0, by = "stid") %>%
    select(distcode.x, schcode.x, schnumb.x, stid, grade.x, pl.x, grade.y, pl.y) %>%
    rename(distcode = distcode.x,
           schcode = schcode.x,
           schnumb = schnumb.x,
           grade01 = grade.x,
           pl01 = pl.x,
           grade = grade.y,
           year00 = pl.y) %>%
    left_join(target, by = c("grade", "year00")) %>%
    mutate(diff = pl01 - year01,
           met = diff >= 0,
           statecode = 0)

head(year0100)

# define function
ELP_indicator <- function(dataset, code) {
    dataset %>%
        select(code, distcode, schcode, stid, diff, met) %>%
        group_by(dataset[[code]]) %>%
        filter(!is.na(diff)) %>%
        summarize(percent_met = mean(met),
                  mean_diff = mean(diff),
                  n_students = n())
}

# calculate school-level, district-level, and state-level rates
school_level <- ELP_indicator(year0100, "schnumb") %>%
    rename(schnumb = `dataset[[code]]`) %>%
    left_join(schools18, by = "schnumb") %>%
    select(schnumb, AGAID, distcode, distname, schcode, schname, HS, 
           percent_met, mean_diff, n_students) %>%
    mutate(total_points = ifelse(HS == "Y", 5, 
                                 ifelse(HS == "N", 10, "no")),
           points = as.numeric(total_points) * percent_met,
           percent_met = percent_met * 100)

head(school_level)

district_level <- ELP_indicator(year0100, "distcode") %>%
    rename(distcode = `dataset[[code]]`) %>%
    left_join(schools18, by = "distcode") %>%
    select(distcode, distname, percent_met, mean_diff, n_students) %>%
    mutate(schnumb = distcode * 1000,
           AGAID = NA,
           schcode = 0,
           schname = "Districtwide",
           HS = NA,
           percent_met = percent_met * 100,
           total_points = NA,
           points = NA)

district_level <- district_level[!duplicated(district_level$distcode), ]

head(district_level)

state_level <- ELP_indicator(year0100, "statecode") %>%
    rename(distcode = `dataset[[code]]`) %>%
    mutate(schcode = 0,
           schnumb = 0,
           AGAID = NA,
           distname = "Statewide",
           schname = "Statewide",
           HS = NA,
           percent_met = percent_met * 100,
           total_points = NA,
           points = NA)

head(state_level)

final <- rbind(school_level, district_level, state_level) %>%
    arrange(schnumb)

head(final)
