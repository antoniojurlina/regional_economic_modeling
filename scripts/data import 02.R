#------------------------------------------------#
# Script that imports the assignment data in     #
# csv format and stores it as RData for future   #
# analysis                                       #
#------------------------------------------------#

#-------- packages --------
library(tidyverse)
library(tidylog)

#-------- data and directory --------
paste0(here::here(), "/data") %>% setwd()

growth_data <- read_csv("growth_data.csv")
multiplier_data <- read_csv("multiplier_data.csv")
multiplier_data_usa <- read_csv("multiplier_data_us.csv")

#-------- "tidy" the data frames --------
multiplier_data <- multiplier_data %>%
  pivot_longer(cols = 4:22, names_to = "year", values_to = "employment")

multiplier_data_usa <- multiplier_data_usa %>%
  pivot_longer(cols = 4:22, names_to = "year", values_to = "employment") %>%
  select(-Region)

multiplier_data <- left_join(multiplier_data, multiplier_data_usa, by = c("Description", "LineCode", "year"))

colnames(growth_data) <- c("region", "year", "labor", "capital", "technology", "gdp")
colnames(multiplier_data) <- c("region", "code", "description", "year", "employment", "employment_us")

multiplier_data <- multiplier_data %>%
  select(region, year, description, employment, employment_us, code)

#-------- saving the data frames --------
save(growth_data, file = "growth_data.RData")
save(multiplier_data, file = "multiplier_data.RData")

