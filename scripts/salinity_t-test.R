library(lme4)
library(here)
library(tidyverse)
library(janitor)
library(lubridate)

# read in data 
salinity <- read_csv(here::here("data", "raw", "Salinity Data.csv")) %>% 
  clean_names() 

# deleting the two rows that are duplicated data
salinity <- salinity[!duplicated(salinity), ]

salinity <- salinity %>% 
  mutate(date = dmy(date),
         site = factor(site),
         salinity_group = factor(salinity_group, levels = c("L", "H")))

# calculate a 10th percentile for each site 
salinity_summary <- salinity %>% 
  group_by(site) %>% 
  mutate(salinity_10percentile = quantile(salinity, probs = 0.1)) %>% 
  select(-c(date, salinity))
  
salinity_summary <- salinity_summary[!duplicated(salinity_summary), ]

# t test 
sal_test <- t.test(salinity_10percentile ~ salinity_group, 
                   data = salinity_summary,
                   var.equal = T)

sal_test
