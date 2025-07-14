# 01_data_cleaning.R
# ===================
# This script handles the data import, cleaning, and preprocessing steps for the aspergillosis Analysis.
# Output: Cleaned datasets saved to /data/processed/

# load libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(readr)

# set working directory (update to your local path)
setwd("~/Oracle Data")

# import data
asp_raw <- read_csv("aspergillosis_data.csv")

# weighted data
weights <- read_csv("weights.csv")
forweights <- read_csv("forweights.csv")

# 01. Prepare the Data
glimpse(asp_raw)
summary(asp_raw)

asp <- asp_raw %>%
  dplyr::select(year, state = prefstate, urban = prefurban, race = prefrace,
                ethnicity = prefethnicity, gender = prefgender, age = age_group,
                n_rwdpts, n_fungal)

# 02. Data Wrangling

# remove "unknown" as a category
asp_na <- as.data.frame(asp) %>%
  mutate_all(~replace(.,. == "Other or Unknown", NA)) %>%
  mutate_all(~replace(.,. == "Unknown", NA)) %>%
  mutate(state = as.factor(state), year = as.integer(year), gender = as.factor(gender), 
         race = as.factor(race), ethnicity = as.factor(ethnicity), age = as.factor(age), 
         urban = as.factor(urban))

# combine asian/pacific islander to reduce race categories
asp_na$race2 <- recode_factor(asp_na$race, 
                              "American Indian or Alaska Native" = "American Indian or Alaska Native",
                              "Asian" = "Asian/Pacific Islander",
                              "Black or African American" = "Black or African American",
                              "Native Hawaiian or Other Pacific Islander" = "Asian/Pacific Islander",
                              "Some Other Race" = "Other",
                              "White or Caucasian" = "White")

# combine counts for new race categories
asp2 <- asp_na %>%
  group_by(state, year, gender, ethnicity, age, urban, race2) %>%
  summarize(n_fungal = sum(n_fungal),
            n_rwdpts = sum(n_rwdpts)) %>%
  ungroup()

# create analytic dataset
df <- asp2 %>%
  mutate(male = as.factor(case_when(gender == 'Male' ~ 1, TRUE ~ 0)), 
         rural = as.factor(case_when(urban == 'rural' ~ 1, TRUE ~ 0))) %>%
  dplyr::select(state, year, male, race2, ethnicity, rural, age, 
                n_fungal, n_rwdpts) %>%
  filter(n_rwdpts > 0) # remove strata where there are no RWD patients meeting those demographics (n_rwdpts is the offset variable and cannot take the log of 0)

# set referent categories
df$race2 <- relevel(df$race2, ref = "White")
df$ethnicity <- relevel(df$ethnicity, ref = "Non-Hispanic")
df$age <- relevel(df$age, ref = "18 to 34")

# define covid variables
df$covid <- ifelse(df$year >=2020, 1, 0)
df$year_center <- df$year - 2019

# remove nas (final check)
df1 <- na.omit(df)

# save cleaned data
write_csv(df1, 'data/processed/aspergillosis_cleaned.csv')