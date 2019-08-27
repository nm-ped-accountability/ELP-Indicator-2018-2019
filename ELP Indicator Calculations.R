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


# Match year0 and year1 ---------------------------------------------------
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

year1 <- year1_raw %>%
    select(test_schnumb, distcode, schcode, stid, grade, PL_composite) %>%
    mutate(schnumb = test_schnumb,
           pl = PL_composite,
           sy = 2018) %>%
    select(distcode, schcode, schnumb, stid, grade, pl, sy)

head(year1)

year01 <- left_join(year1, year0, by = "stid")
head(year01)
