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

schools <- read.csv("Master Schools 2019 V3.csv",
                    header = TRUE, stringsAsFactors = FALSE)

schools18 <- schools[schools$SY == 2018, ]
head(schools)


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
           met = diff >= 0)

head(year0100)

# caclulate ELP points
ELP_points <- function(dataset, code) {
    rates <- dataset %>%
        select(code, distcode, schcode, stid, diff, met) %>%
        group_by(dataset[[code]]) %>%
        filter(!is.na(diff)) %>%
        summarize(percent_met = mean(met),
                  mean_diff = mean(diff),
                  n_students = n())
    rates
}

school_level <- ELP_points(year0100, "schnumb")

head(school_level)


