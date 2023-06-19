#### Simper analysis on transect data

library(tidyverse)
library(skimr)
library(ggbiplot)
library(vegan)
library(here)
library(permute)

theme_set(theme_classic())


# Data prep ---------------------------------------------------------------

# read in data
transect <- read_csv(here::here("data", "tidy", "field_transect_tidy.csv"))

#creating a unique name for each transect to be used as row names 
transect <- transect %>% 
  select(-salinity) %>% 
  unite(col = "site_rep_month", c("site", "quadrat_no", "month"), sep = "_", remove = FALSE) %>% 
   mutate(site = factor(site, levels = c("EC", "HS", "RP", "HB", "LB", "RS")),
         month = factor(month, levels = c("May", "June", "July", "September")),
         region = factor(region, levels = c("Low", "High")))

# parse out only the species data, and getting rid of redundant variables  
survey_comm_data <- transect %>% 
  select(balanus_pt:ulva_spp_pt) 

# recoding factors 
survey_env_data <- transect %>%
  select(month, site, region) %>% 
  mutate(month = recode_factor(month, May = "May", June = "June", July = "July", September = "August"))

mean_abundance <- transect %>% 
  group_by(region) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  select(-quadrat_no)

# Simper ------------------------------------------------------------------

# permutation scheme that maintains the dependence of quadrats along a transect
perm <- how(within = Within(type = "free", mirror = TRUE), 
            plots = Plots(strata = transect$site, mirror = TRUE, type = "free"), 
            blocks = NULL) 


# simper analysis 
transect_salinity_species_contributions <- simper(wisconsin(survey_comm_data), 
                                                  group = survey_env_data$region, 
                                                  permutations = perm)

simper_output <- summary(transect_salinity_species_contributions)$High_Low

indices <- which(simper_output$cumsum <= 0.70)

taxon <- rownames(simper_output)[indices]
avg <- round(simper_output$average[indices]*100, 1)
cumsum <- round(simper_output$cumsum[indices]*100, 1)

avga <- mean_abundance %>% filter(region == "L") %>% select(contains(taxon)) %>% t()
avgb <- mean_abundance %>% filter(region == "H") %>% select(contains(taxon)) %>% t()

df <- tibble(taxon = taxon,
                         `avg contribution (%)` = avg,
                         `cumulative contribution (%)` = cumsum,
                         `mean abundance low salinity` = avga,
                         `mean abundance high salinity` = avgb
)


df$taxon[df$taxon == "mytilus_pt"] <- "Mytilus trossulus (%)"
df$taxon[df$taxon == "chthamalus_pt"] <- "Chthamalus dalli (%)"
df$taxon[df$taxon == "balanus_pt"] <- "Balanus glandula (%)"
df$taxon[df$taxon == "fucus_pt"] <- "Fucus distichus (%)"
df$taxon[df$taxon == "mastocarpus_crust_pt"] <- "Petrocelis (%)"
df$taxon[df$taxon == "paradigitalis_no"] <- "Lottia paradigitalis (no.)"
df$taxon[df$taxon == "barnacle_recruits_pt"] <- "Barnacle recruits (%)"
df$taxon[df$taxon == "pelta_no"] <- "Lottia pelta (no.)"


df %>%   
  mutate(across("avg contribution (%)":"cumulative contribution (%)", round, 2)) %>% 
  mutate(across("mean abundance low salinity":"mean abundance high salinity", round, 1)) %>%
  kableExtra::kable(align = "lccrr") %>% 
  kableExtra::kable_classic() %>% 
  kableExtra::kable_styling(full_width = F,
                            position = "left") %>% 
  kableExtra::save_kable("./figures/simper_table_transects.png")


# transect_month_species_contributions <- simper(wisconsin(survey_comm_data), group = survey_env_data$month, permutations = perm)


