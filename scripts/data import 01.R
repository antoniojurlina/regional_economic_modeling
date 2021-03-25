#------------------------------------------------#
# Script that imports the assignment data in     #
# csv format and stores it as RData for future   #
# analysis                                       #
#------------------------------------------------#

#-------- packages --------
library(readr)
library(dplyr)
library(tidyr)

#-------- data and directory --------
directory <- paste0(here::here(), "/data") %>% setwd()

state_csv <- "state data.csv"
us_csv <- "us data.csv"

state_data_prelim <- read_csv(state_csv)
us_data_prelim <- read_csv(us_csv)

# change column names
colnames(state_data_prelim) <- c("region", "sector", "2017", "2009", "level")
colnames(us_data_prelim) <- c("sector", "2017", "2009", "level")

#-------- "tidy" and save the data frame --------

# create a year column and sort data accordingly
state_data_prelim <- state_data_prelim %>%
  pivot_longer(cols = 3:4, names_to = "year", values_to = "employment") %>%
  mutate(year = as.integer(year),
         region = tolower(region))

us_data_prelim <- us_data_prelim %>%
  pivot_longer(cols = 2:3, names_to = "year", values_to = "us_employment") %>%
  mutate(year = as.integer(year))

# separate the sector column into three, format name (NAICS), code and sector name
state_data_prelim <- separate(state_data_prelim, sector, c("format", "code", "sector"), sep = " ", extra = "merge")
us_data_prelim <- separate(us_data_prelim, sector, c("format", "code", "sector"), sep = " ", extra = "merge")

# add state_employment levels
state_data_fixed <- state_data_prelim %>%
  group_by(region, year, level) %>%
  mutate(state_employment = sum(employment)) %>%
  ungroup()

# add us employment levels
us_data_fixed <- us_data_prelim %>%
  group_by(level, year) %>%
  mutate(us_employment_total = sum(us_employment)) %>%
  ungroup()

# add US values as a whole to the initial data
project_1 <- left_join(state_data_fixed, us_data_fixed)

# save the data frame for further analysis
save(project_1, file = "project 01.RData")



