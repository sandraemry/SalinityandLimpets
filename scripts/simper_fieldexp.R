#### Simper analysis on field exclusion data

library(tidyverse)
library(skimr)
library(ggbiplot)
library(vegan)
library(here)
library(permute)

theme_set(theme_classic())


# Data Prep ---------------------------------------------------------------

exp <- read_csv(here::here("data", "tidy", "field_exclusion_tidy.csv"))

exp <- exp %>% 
  mutate(treatment = factor(treatment), 
                region = factor(region),
                site = factor(site)) %>% 
  unite(interaction, c(region, treatment), sep = "_", remove = F) %>% 
  select(-c(salinity, date, time, month, replicate, greens_pyropia, reds, reds_fucus, greens, 
                   littorina_spp_no, paradigitalis_no, unknown_limpets_no))

exp_env_data <- exp %>% 
  select(treatment, region, site, interaction)

exp_comm_data <- exp %>%
  select(balanus_no:fucus_pt)

mean_abundance <- exp %>% 
  unite(interaction, c(region, treatment), sep = "_", remove = F) %>% 
  group_by(interaction) %>% 
  summarise(across(where(is.numeric), mean))


# Simper ------------------------------------------------------------------

# perm <- how(within = Within(type = "free"), 
#             plots = Plots(strata = exp_env_data$site, type = "free"), 
#             blocks = NULL) 
# 
# # simper analysis on salinity region 
# exp_salinity_species_contributions <- simper(wisconsin(exp_comm_data), group = exp_env_data$region, permutations = perm)
# 
# ## simper analysis on experiment data by treatment 
# exp_treatment_species_contributions <- simper(wisconsin(exp_comm_data), group = exp_env_data$treatment, permutations = perm)
# 
# # put results in a tibble
# salinity_species_contribution <- tibble(species = exp_salinity_species_contributions$High_Low$species,
#                                         contribution = exp_salinity_species_contributions$High_Low$average, group = rep("salinity", 12))
# 
# treatment_species_contribution <- tibble(species = exp_treatment_species_contributions$E_C$species,
#                                          contribution = exp_treatment_species_contributions$E_C$average, group = rep("treatment", 12))
# 
# t1 <- treatment_species_contribution %>% 
#   group_by(group) %>% 
#   slice_max(contribution, n = 4) 
# 
# # arrange by highest to lowest contributors
# t2 <- salinity_species_contribution %>% 
#   slice_max(contribution, n = 4) 


# Simper analysis with all 4 groups at once -------------------------------

perm <- how(within = Within(type = "free"), 
            plots = Plots(strata = exp_env_data$site, type = "free"), 
            blocks = NULL) 

exp_no_grazers <- simper(wisconsin(exp_comm_data), group = exp_env_data$interaction, permutations = perm)

##################### High exclusion vs. High control ##################### 

he_hc <- summary(exp_no_grazers)$High_exclusion_High_control
indices <- which(he_hc$cumsum <= 0.8)

taxon <- rownames(he_hc)[indices]
avg <- round(he_hc$average[indices]*100, 1)
cumsum <- round(he_hc$cumsum[indices]*100, 1)

avga <- mean_abundance %>% filter(interaction == "High_exclusion") %>% select(contains(taxon)) %>% t()
avgb <- mean_abundance %>% filter(interaction == "High_control") %>% select(contains(taxon)) %>% t()

simper_table_1 <- tibble(comparison = c("high salinity - ", "", "high salinity + "),
                         taxon = taxon,
                         `avg contribution (%)` = avg,
                         `cumulative contribution (%)` = cumsum,
                         `mean abundance group a` = avga,
                         `mean abundance group b` = avgb
                           )

##################### High exclusion vs. Low control ##################### 

he_lc <- summary(exp_no_grazers)$High_exclusion_Low_control
indices <- which(he_lc$cumsum <= 0.8)

taxon <- rownames(he_lc)[indices]
avg <- round(he_lc$average[indices]*100, 1)
cumsum <- round(he_lc$cumsum[indices]*100, 1)

avga <- mean_abundance %>% filter(interaction == "High_exclusion") %>% select(contains(taxon)) %>% t()
avgb <- mean_abundance %>% filter(interaction == "Low_control") %>% select(contains(taxon)) %>% t()

simper_table_2 <- tibble(comparison = c("high salinity - ", "", "low salinity + ", ""),
                         taxon = taxon, 
                         `avg contribution (%)` = avg,
                         `cumulative contribution (%)` = cumsum,
                         `mean abundance group a` = avga,
                         `mean abundance group b` = avgb
)



##################### High exclusion vs. Low exclusion ##################### 

he_le <- summary(exp_no_grazers)$High_exclusion_Low_exclusion
indices <- which(he_le$cumsum <= 0.8)

taxon <- rownames(he_le)[indices]
avg <- round(he_le$average[indices] * 100, 1)
cumsum <- round(he_le$cumsum[indices] * 100, 1)

avga <- mean_abundance %>% filter(interaction == "High_exclusion") %>% select(contains(taxon)) %>% t()
avgb <- mean_abundance %>% filter(interaction == "Low_exclusion") %>% select(contains(taxon)) %>% t()

simper_table_3 <- tibble(comparison = c("high salinity - ", "", "low salinity - "),
                         taxon = taxon, 
                         `avg contribution (%)` = avg,
                         `cumulative contribution (%)` = cumsum,
                         `mean abundance group a` = avga,
                         `mean abundance group b` = avgb
)

##################### High control vs. Low control ##################### 

hc_lc <- summary(exp_no_grazers)$High_control_Low_control
indices <- which(hc_lc$cumsum <= 0.8)

taxon <- rownames(hc_lc)[indices]
avg <- round(hc_lc$average[indices]*100, 1)
cumsum <- round(hc_lc$cumsum[indices]*100, 1)

avga <- mean_abundance %>% filter(interaction == "High_control") %>% select(contains(taxon)) %>% t()
avgb <- mean_abundance %>% filter(interaction == "Low_control") %>% select(contains(taxon)) %>% t()

simper_table_4 <- tibble(comparison = c("high salinity + ", "", "low salinity + "),
                         taxon = taxon, 
                         `avg contribution (%)` = avg,
                         `cumulative contribution (%)` = cumsum,
                         `mean abundance group a` = avga,
                         `mean abundance group b` = avgb
)

##################### High control vs. Low exclusion ##################### 

hc_le <- summary(exp_no_grazers)$High_control_Low_exclusion
indices <- which(hc_le$cumsum <= 0.8)

taxon <- rownames(hc_le)[indices]
avg <- round(hc_le$average[indices]*100, 1)
cumsum <- round(hc_le$cumsum[indices]*100, 1)

avga <- mean_abundance %>% filter(interaction == "High_control") %>% select(contains(taxon)) %>% t()
avgb <- mean_abundance %>% filter(interaction == "Low_exclusion") %>% select(contains(taxon)) %>% t()

simper_table_5 <- tibble(comparison = c("high salinity + ", "", "low salinity - "),
                         taxon = taxon, 
                         `avg contribution (%)` = avg,
                         `cumulative contribution (%)` = cumsum,
                         `mean abundance group a` = avga,
                         `mean abundance group b` = avgb
)

##################### Low control vs. Low exclusion ##################### 

lc_le <- summary(exp_no_grazers)$Low_control_Low_exclusion
indices <- which(lc_le$cumsum <= 0.8)

taxon <- rownames(lc_le)[indices]
avg <- round(lc_le$average[indices] * 100, 1)
cumsum <- round(lc_le$cumsum[indices] * 100, 1)

avga <- mean_abundance %>% filter(interaction == "Low_control") %>% select(contains(taxon)) %>% t()
avgb <- mean_abundance %>% filter(interaction == "Low_exclusion") %>% select(contains(taxon)) %>% t()

simper_table_6 <- tibble(comparison = c("low salinity + ", "", "low salinity - "),
                         taxon = taxon, 
                         `avg contribution (%)` = avg,
                         `cumulative contribution (%)` = cumsum,
                         `mean abundance group a` = avga,
                         `mean abundance group b` = avgb
)


##################### combining tables ##################### 

df <- rbind(simper_table_1, simper_table_2, simper_table_3, simper_table_4, simper_table_5, simper_table_6)

df$taxon[df$taxon == "ulva_pt"] <- "Ulva sp. (%)"
df$taxon[df$taxon == "chthamalus_no"] <- "Chthamalus dalli (no.)"
df$taxon[df$taxon == "balanus_no"] <- "Balanus glandula (no.)"
df$taxon[df$taxon == "fucus_pt"] <- "Fucus distichus (%)"
df$taxon[df$taxon == "diatom_pt"] <- "Diatoms (%)"

df %>%   
  select(-comparison) %>% 
  mutate(across("avg contribution (%)":"cumulative contribution (%)", round, 2)) %>% 
  mutate(across("mean abundance group a":"mean abundance group b", round, 1)) %>%
  kableExtra::kable(align = "lccrr") %>% 
  kableExtra::kable_classic() %>% 
  kableExtra::kable_styling(full_width = F,
                            position = "left") %>% 
  kableExtra::pack_rows("high salinity - vs. high salinity +", 1, 3) %>%
  kableExtra::pack_rows("high salinity - vs. low salinity +", 4, 7) %>% 
  kableExtra::pack_rows("high salinity - vs. low salinity -", 8, 10) %>%
  kableExtra::pack_rows("high salinity + vs. low salinity +", 11, 13) %>% 
  kableExtra::pack_rows("high salinity + vs. low salinity -", 14, 16) %>% 
  kableExtra::pack_rows("low salinity + vs. low salinity -", 17, 19) %>% 
  kableExtra::save_kable("./figures/simper_table_fieldexp.png")
