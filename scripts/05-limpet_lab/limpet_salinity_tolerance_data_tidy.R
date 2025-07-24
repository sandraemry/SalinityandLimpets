# Limpet salinity tolerance data tidying

# Lottia pelta from low and high sites
raw_mort <- read_csv("./data/raw/limpet_mortality_local_adaptation.csv")

raw_mort <- raw_mort %>% 
  clean_names() %>% 
  select(site:tank) %>% 
  mutate(status = 1) %>% 
  rename(day = days_survived) %>% 
  arrange(salinity, tank, site) %>% 
  mutate(site = factor(site, levels = c("Low", "High"))) %>% 
  as.data.frame() %>% 
  mutate(salinity = factor(salinity, levels = c("5", "8", "11", "14", "17", "20")))

raw_mort$status[raw_mort$day == 50] <- 0

raw_mort$day[raw_mort$day == 50]  <- 28

write_csv(raw_mort, "./data/tidy/pelta_salinity_tolerance.csv")


# tidal emersion salinity tolerance experiment --------------------------

# Lottia pelta

pelta_data <- read_csv("./data/raw/limpet_salinity_tolerance_tidal_pelta_exp.csv") %>% 
  clean_names()

head(pelta_data)

pelta_data <- pelta_data %>% 
  mutate(tide_treatment = factor(tide_treatment),
         salinity = factor(salinity, levels = c("10", "20", "30"))) %>% 
  filter(tank != "Table", !is.na(tank)) %>% 
  mutate(species = "Lottia_pelta") %>% 
  mutate(status = 1) %>% 
  rename(day = number_days_survived) %>% 
  select(species, tide_treatment, salinity, day, tank, status)

pelta_data$status[pelta_data$day > 28] <- 0

pelta_data$day[pelta_data$day > 28]  <- 28

pelta_data %>% 
  arrange(species, tide_treatment, salinity, tank, day, status)

# Lottia digitalis
digitalis_data <- read_csv("./data/raw/limpet_salinity_tolerance_tidal_digitalis_exp.csv") %>% 
  clean_names()

head(digitalis_data)

digitalis_data <- digitalis_data %>% 
  mutate(tide_treatment = factor(tide_treatment),
         salinity = factor(salinity, levels = c("10", "20", "30"))) %>% 
  filter(tank != "No Tank", !is.na(tank)) %>% 
  mutate(species = "Lottia_digitalis") %>% 
  mutate(status = 1) %>% 
  rename(day = number_days_survived) %>% 
  select(species, tide_treatment, salinity, day, tank, status)

digitalis_data$status[digitalis_data$day > 28] <- 0

digitalis_data$day[digitalis_data$day > 28]  <- 28

digitalis_data$species <- "Lottia_digitalis"

digitalis_data <- digitalis_data %>% 
  arrange(species, tide_treatment, salinity, tank, day, status)

limpet_emersion_salinity <- rbind(pelta_data, digitalis_data)

write_csv(limpet_emersion_salinity, "./data/tidy/limpet_emersion_salinity_tolerance.csv")

