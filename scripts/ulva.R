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
         site = factor(site, levels = c("EC", "HS", "RP", "HB", "LB", "RS"))) %>% 
  select(region, site, treatment, fucus_pt, ulva_spp_pt, balanus_no, chthamalus_no, masto_crust_pt, diatom_pt)


# Fit models -----------------------------------------------------------------

hist(exp_data$ulva_spp_pt) # lots of zeros 

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
exp_ulva_tweedie <- glmmTMB(ulva_spp_pt ~ treatment * region +  (1|region/site), data = exp_data, family = tweedie(link = "log"))

# exp_ulva_beta <- glmmTMB(ulva_spp_pt ~ treatment * region +  (1|region/site), data = exp_data, family = beta_family(link = "logit"))

#checking assumptions 
set.seed(19)
exp_ulva_sim_res <- simulateResiduals(exp_ulva_tweedie)
plot(exp_ulva_sim_res) # not good

par(mfrow = c(1,2))
plotResiduals(exp_ulva_sim_res, form = exp_data$treatment) #  good
plotResiduals(exp_ulva_sim_res, form = exp_data$region) # not good

#update model
exp_ulva_tweedie_2 <- glmmTMB(ulva_spp_pt ~ treatment * region +  (1|region/site), data = exp_data, 
                         dispformula = ~region, family = tweedie(link = "log"))

#checking assumptions 
set.seed(21)
exp_ulva_sim_res <- simulateResiduals(exp_ulva_tweedie_2)

plot(exp_ulva_sim_res) # good

par(mfrow = c(1,2))
testOverdispersion(exp_ulva_sim_res)
testZeroInflation(exp_ulva_sim_res)

# Model outputs -----------------------------------------------------------

# parameters
coef <- summary(exp_ulva_tweedie_2)$coefficients$cond
  
coef %>% 
  kableExtra::kable(caption = "coefficient estimates for model fitted to Ulva sp. field experiment data") %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
  
car::Anova(exp_ulva_tweedie_2, type = "III") %>% 
  tidy() %>% 
  kableExtra::kable(caption = "p values for model fitted to Ulva sp. field experiment data") %>% 
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

exp_ulva_anova_table <- car::Anova(exp_ulva_tweedie, type = "III") %>% 
  tidy()

grp_means_log <- emmeans(exp_ulva_tweedie, specs = pairwise ~ treatment | region, data = exp_data)

pw <- as.data.frame(grp_means_log$contrasts)

pw %>%   
  kableExtra::kable(caption = "tukey adjusted p values for pairwise comparisons from model fitted to Chthamalus field experiment data") %>% 
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left") %>% 
  kableExtra::column_spec(1:7, color = "black", background = ifelse(pw$p.value < 0.05, "pink", "white"))

plot(grp_means_log, comparison = TRUE)





