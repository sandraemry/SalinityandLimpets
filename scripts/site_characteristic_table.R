# Site Characteristic table 

library(tidyverse)
library(here)

site_characteristics <- read_csv(here("data", "raw", "site_characteristics.csv")) 

# correct aspect from magnetic north to true north 
site_characteristics <- site_characteristics %>% 
  separate(`Aspect (°)`, into = c("first", "second"), sep = "-", remove = T) %>% 
  mutate(first = as.numeric(first) + 15,
         second = as.numeric(second) + 15) %>% 
  unite(`Aspect (°)`, c(first, second), sep = "-")

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

site_characteristics <- site_characteristics %>% 
  separate(Latitude, into = c("d", "m", "s"), sep = c(3,5)) %>% 
  mutate(d = str_extract(d, "[:digit:]{2}")) %>% 
  mutate(s = str_extract(s, "[:digit:]{2}")) %>% 
  mutate(across(d:s, ~as.numeric(.))) %>% 
  unite(Latitude, c(d, m, s), sep = " ") %>% 
  separate(Longitude, into = c("d", "m", "s"), sep = c(4,7)) %>% 
  mutate(d = str_extract(d, "[:digit:]{3}")) %>% 
  mutate(m = str_extract(m, "[:digit:]{2}")) %>% 
  mutate(s = str_extract(s, "[:digit:]{2}")) %>% 
  mutate(across(d:s, ~as.numeric(.))) %>% 
  unite(Longitude, c(d, m, s), sep = " ")
  
site_characteristics$Latitude <- angle2dec(site_characteristics$Latitude)
site_characteristics$Longitude <- angle2dec(site_characteristics$Longitude)

site_characteristics$Site <- str_replace(site_characteristics$Site, "Rope Site", "Sharon Cove")

site_characteristics %>% 
  kableExtra::kable(align = "lccccr") %>% 
  kableExtra::kable_classic() %>% 
  kableExtra::kable_styling(full_width = F,
                            position = "left") %>% 
  kableExtra::save_kable("./figures/site_characteristics.png")

write.table(site_characteristics, file = './tables/site_characteristics.csv', sep = ",")


