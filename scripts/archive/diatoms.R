### GLM of Ulva sp. for the field experiment 

library(skimr)
library(visreg)
library(tidyverse)
library(broom)
library(broom.mixed)
library(dotwhisker)
library(rcompanion)
library(effects)
library(xtable)
library(emmeans)
library(bbmle) 
library(car)
library(here)
library(glmmTMB)
library(DHARMa)

exp_data <- read_csv(here::here("data", "tidy", "field_exclusion_tidy.csv"))

exp_data <- exp_data %>% 
  mutate(treatment = factor(treatment, levels = c("control", "exclusion")),
         region = factor(region, levels = c("Low", "High")),
         site = factor(site, levels = c("Lions Bay 2", "Rope Site 2", "Horseshoe Bay", "Ruckle", "Eagle Cove", "Hailstorm 2"))) %>% 
  select(region, site, treatment, fucus_pt, ulva_pt, balanus_no, chthamalus_no, masto_crust_pt, diatom_pt)


# Fit models -----------------------------------------------------------------

hist(exp_data$diatom_pt) # lots of zeros 

# changing percent cover to proportion
# transforming 0s and 1s according to Smithson M, Verkuilen J (2006). "A Better Lemon Squeezer? 
# Maximum-Likelihood Regression with Beta-Distributed Dependent Variables." Psychological Methods, 11 (1), 54–71.

# (y * (n−1) + 0.5) / n where n is the sample size.
# n <- 7
# 
# exp_data <- exp_data %>% 
#   mutate(across(ends_with("_pt"), ~ . /100)) %>% 
#   mutate(across(ends_with("_pt"), ~ (. * (n-1) + 0.5)/n))

# fit models 
exp_diatom_tweedie <- glmmTMB(diatom_pt ~ treatment * region +  (1|region/site), data = exp_data, family = tweedie(link = "log"))

# exp_diatom_beta <- glmmTMB(ulva_pt ~ treatment * region +  (1|region/site), data = exp_data, family = beta_family(link = "logit"))

#checking assumptions 
set.seed(17)
exp_diatom_sim_res <- simulateResiduals(exp_diatom_tweedie)
plot(exp_diatom_sim_res) # not good

par(mfrow = c(1,2))
plotResiduals(exp_diatom_sim_res, form = exp_data$treatment) #  good
plotResiduals(exp_diatom_sim_res, form = exp_data$region) #  good

par(mfrow = c(1,2))
testOverdispersion(exp_diatom_sim_res)
testZeroInflation(exp_diatom_sim_res)

# Model outputs -----------------------------------------------------------

# parameters
coef <- summary(exp_diatom_tweedie)$coefficients$cond

coef %>% 
  kableExtra::kable(caption = "coefficient estimates for model fitted to Diatom cover from field experiment data") %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

car::Anova(exp_diatom_tweedie, type = "II") %>% 
  tidy() %>% 
  kableExtra::kable(caption = "p values for model fitted to Ulva sp. field experiment data") %>% 
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

exp_diatom_anova_table <- car::Anova(exp_diatom_tweedie, type = "II") %>% 
  tidy()






