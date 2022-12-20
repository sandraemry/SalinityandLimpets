# This script statistically analyses the differences in grazer abundance
# among treatments in the field exclusion experiment: 

library(tidyverse)
library(glmmTMB)
library(bbmle)
library(DHARMa)

# Data prep and initial visualization -------------------------------------

exp_grazers <- read_csv(here::here("data", "raw", "Raw_Count_Data.csv"))

# More littorines in control plot than exclusion in high salinity region 
exp_grazers %>% filter(Treatment != "I") %>% ggplot(data = .) + geom_boxplot(aes(x = Treatment, y = log(`Total Littorina (#)` + 1)))

# same amount, but then more limpets were taken out... ?
exp_grazers %>% filter(Treatment != "I") %>% ggplot(data = .) + geom_boxplot(aes(x = Treatment, y = log(`Total Limpets in Ring Before adding/subtracting` + 1)))

exp_grazers %>% filter(Treatment != "I") %>% ggplot(data = .) + geom_boxplot(aes(x = Region, y = log(`Total Littorina (#)` + `Total Limpets in Ring Before adding/subtracting` + 1), fill = Treatment))

exp_grazers <- exp_grazers %>% 
  filter(Month != "September") %>% 
  filter(Treatment != "I") %>% 
  mutate(Treatment = factor(Treatment, levels = c("C", "E")),
         Region = factor(Region, levels = c("Low", "High"))) %>% 
rename(littorines =  `Total Littorina (#)`, limpets = `Total Limpets in Ring Before adding/subtracting`) %>% 
  select(Month:Treatment, littorines, limpets) %>% 
  group_by(Region, Site, Replicate, Treatment) %>% 
  summarise(across(where(is.numeric), sum)) # sum grazers in each plot at the 3 different census dates
  
  
names(exp_grazers) <- tolower(names(exp_grazers))

exp_grazers$treatment <- str_replace(exp_grazers$treatment, "C", "control") 
exp_grazers$treatment <- str_replace(exp_grazers$treatment, "E", "exclusion")

# what species do we have?
# exp_grazers %>%
#   summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% View
  

# Fit model to littorines ---------------------------------------

littorine_mod_NB2 <- glmmTMB(littorines ~ treatment + (1|region/site), 
        data = exp_grazers, family = nbinom2) # best model

littorine_mod_NB1 <- update(littorine_mod_NB2, family = nbinom1)

littorine_mod_ziNB2 <-  glmmTMB(littorines ~ treatment + (1|region/site), 
                              data = exp_grazers, ziformula = ~1, family = nbinom2)

littorine_mod_ziNB1 <- update(exp_grazers_ziNB2, family = nbinom1) 


AICtab(littorine_mod_NB2, littorine_mod_NB1, littorine_mod_ziNB1)

set.seed(20220315)

littorine_residuals <- simulateResiduals(littorine_mod_NB2)
plot(littorine_residuals)

car::Anova(littorine_mod_NB2) # treatment is significant - lower in exclosures


# Fit model to limpets ---------------------------------------

limpets_mod_NB2 <- glmmTMB(limpets ~ treatment + (1|region/site), 
                             data = exp_grazers, family = nbinom2) # best model

limpets_mod_NB1 <- update(limpets_mod_NB2, family = nbinom1)

limpets_mod_ziNB2 <-  glmmTMB(limpets ~ treatment + (1|region/site), 
                                data = exp_grazers, ziformula = ~1, family = nbinom2)

limpets_mod_ziNB1 <- update(limpets_mod_ziNB2, family = nbinom1) 


AICtab(limpets_mod_NB2, limpets_mod_NB1, limpets_mod_ziNB2, limpets_mod_ziNB1)

set.seed(20220315)

limpet_residuals <- simulateResiduals(limpets_mod_NB2)
plot(limpet_residuals)

car::Anova(limpets_mod_NB2) # treatment not significant

##

# Plots -------------------------------------------------------------------


a <- ggplot(data = exp_grazers) + geom_boxplot(aes(x = treatment, y = log(limpets + 1))) + 
  theme_classic() + 
  labs(x = "grazer treatment", y = "limpets (log(N + 1))")

b <- ggplot(data = exp_grazers) + geom_boxplot(aes(x = treatment, y = log(littorines + 1))) + 
  theme_classic() + 
  labs(x = "grazer treatment", y = "littorines (log(N + 1))")

grazer_plot <- a + b

ggsave("./figures/grazer_by_treatment.png", grazer_plot)

# Abundance to Biomass ----------------------------------------------------


# Lottia pelta: log wt (g) = 3.516 log length (cm) - 2.010 (Dethier & Duggins 1988)


