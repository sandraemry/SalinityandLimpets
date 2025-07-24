### This script will serve to calculate and model data needed to respond to reviewer comments 
library(tidyverse)

# Comment 1: Ulva is not common in the transects 
# But it was common in the control and 'inclusion' plots of the field experiment 

data <- read_csv("./data/tidy/field_exclusion_tidy.csv")

data %>% 
  select(date:treatment, ulva_spp_pt) %>% 
  filter(treatment != "exclusion") %>% 
  group_by(region) %>% 
  summarise(avg_ulva = mean(ulva_spp_pt), se_ulva = sd(ulva_spp_pt)/sqrt(21))

data %>% 
  select(date:treatment, ulva_spp_pt) %>% 
  filter(treatment != "exclusion") %>% 
  group_by(region) %>% 
  ggplot(.) + 
  geom_boxplot(aes(x = region, y = ulva_spp_pt)) + 
  geom_point(aes(x = region, y = ulva_spp_pt), alpha = 0.5, position = position_jitter()) 
  
