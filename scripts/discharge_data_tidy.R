library(tidyverse)

discharge <- read_csv(here::here("data", "raw", "Discharge.csv")) %>% 
  clean_names()

discharge <- discharge %>% 
  select(date, discharge)

write_csv(discharge, "./data/tidy/discharge.csv")
