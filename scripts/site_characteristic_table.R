# Site Characteristic table 

library(tidyverse)
library(here)

site_characteristics <- read_csv(here("data", "raw", "site_characteristics.csv"))

site_characteristics %>% 
  kableExtra::kable(align = "lccccr") %>% 
  kableExtra::kable_classic() %>% 
  kableExtra::kable_styling(full_width = F,
                            position = "left") %>% 
  kableExtra::save_kable("./figures/site_characteristics.png")
