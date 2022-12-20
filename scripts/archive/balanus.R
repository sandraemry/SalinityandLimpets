### GLM of Balanus glandula for the field experiment 

library(skimr)
library(cowplot)
library(visreg)
library(tidyverse)
library(broom)
library(broom.mixed)
library(dotwhisker)
library(rcompanion)
library(effects)
library(xtable)
library(lme4)
library(emmeans)
library(glmmTMB)
library(bbmle) 
library(patchwork)
library(DHARMa)
library(car)
library(here)

exp_data <- read_csv(here::here("data", "tidy", "field_exclusion_tidy.csv"))

exp_data <- exp_data %>% 
  mutate(treatment = factor(treatment, levels = c("control", "exclusion")),
         region = factor(region, levels = c("Low", "High")),
         site = factor(site, levels = c("Lions Bay 2", "Rope Site 2", "Horseshoe Bay", "Ruckle", "Eagle Cove", "Hailstorm 2"))) %>% 
  select(region, site, treatment, fucus_pt, ulva_pt, balanus_no, chthamalus_no, masto_crust_pt, diatom_pt)


# Fit models -----------------------------------------------------------------

# fit models 
exp_balanus_poisson <- glmmTMB(round(balanus_no) ~ treatment * region +  (1|region/site), data = exp_data, family = poisson(link = "log"))

exp_balanus_NB <- update(exp_balanus_poisson, family = nbinom2) 

exp_balanus_NB1 <- update(exp_balanus_poisson, family = nbinom1) 

exp_balanus_tweedie <- update(exp_balanus_poisson, family = tweedie) # best model 

# model selection 
AICtab(exp_balanus_poisson, exp_balanus_NB, exp_balanus_NB1, exp_balanus_tweedie)

#checking assumptions 
set.seed(4343)
exp_balanus_sim_res <- simulateResiduals(exp_balanus_NB1)

plot(exp_balanus_sim_res) # unequal within group variance 
# plot residuals against treatment

par(mfrow = c(1,2))
plotResiduals(exp_balanus_sim_res, form = exp_data$treatment) # more variance in control plots
# plot residuals against salinity region
plotResiduals(exp_balanus_sim_res, form = exp_data$region) # more variance in the low salinity sites

### Modelling the dispersion 
# adding a dispersion formula to tweedie distribution
exp_balanus_tweedie_2 <- glmmTMB(balanus_no ~ treatment * region +  (1|region/site), data = exp_data, dispformula = ~treatment*region, family = tweedie(link = "log")) 
exp_balanus_NB1_2 <- glmmTMB(round(balanus_no) ~ treatment * region +  (1|region/site), data = exp_data, dispformula = ~treatment*region, family = nbinom1(link = "log")) 

set.seed(2021)
exp_balanus_sim_res_2 <- simulateResiduals(exp_balanus_NB1_2)

par(mfrow = c(1,2))
plotResiduals(exp_balanus_sim_res_2, form = exp_data$treatment) # good
plotResiduals(exp_balanus_sim_res_2, form = exp_data$region) # not good

# Trying to add a dispersion parameter for each site*trmt combo
exp_balanus_tweedie_3 <- glmmTMB(balanus_no ~ treatment * region +  (1|region/site), data = exp_data, dispformula = ~treatment*site, family = tweedie(link = "log")) # convergence issues 
exp_balanus_NB1_3 <- glmmTMB(round(balanus_no) ~ treatment * region +  (1|region/site), data = exp_data, dispformula = ~treatment*site, family = nbinom1(link = "log")) 

## Fitting site as a fixed effect instead of salinity 
# fit models 
exp_balanus_poisson <- glmmTMB(round(balanus_no) ~ treatment * site, data = exp_data, family = poisson(link = "log"))

exp_balanus_NB2 <- update(exp_balanus_poisson, family = nbinom2) 

exp_balanus_NB1 <- update(exp_balanus_poisson, family = nbinom1) # best model 

exp_balanus_tweedie <- update(exp_balanus_poisson, family = tweedie) 

# model selection 
AICtab(exp_balanus_poisson, exp_balanus_NB, exp_balanus_NB1, exp_balanus_tweedie)

# check assumptions
set.seed(2022)
exp_balanus_sim_res_3 <- simulateResiduals(exp_balanus_NB1)
exp_balanus_sim_res_3 <- simulateResiduals(exp_balanus_tweedie)

par(mfrow = c(1,2))
plotResiduals(exp_balanus_sim_res_3, form = exp_data$treatment) # not good
plotResiduals(exp_balanus_sim_res_3, form = exp_data$site) # not good

# modelling dispersion 

exp_balanus_NB1_2 <- glmmTMB(round(balanus_no) ~ treatment * site, data = exp_data, dispformula = ~treatment*site, family = nbinom1(link = "log"))
exp_balanus_tweedie_2 <- glmmTMB(round(balanus_no) ~ treatment * site, data = exp_data, dispformula = ~treatment*site, family = tweedie(link = "log"))

# check assumptions
set.seed(20)
exp_balanus_sim_res_4 <- simulateResiduals(exp_balanus_NB1_2)

par(mfrow = c(1,2))
plotResiduals(exp_balanus_sim_res_4, form = exp_data$treatment) #  good
plotResiduals(exp_balanus_sim_res_4, form = exp_data$site) #  good


# Model outputs -----------------------------------------------------------


# parameters
coef <- summary(exp_balanus_NB1_2)$coefficients$cond

coef %>% 
  kableExtra::kable(caption = "coefficient estimates for model fitted to Balanus field experiment data") %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

car::Anova(exp_balanus_NB1_2, type = "III") %>% 
  tidy() %>% 
  kableExtra::kable(caption = "p values for model fitted to Balanus field experiment data") %>% 
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

## The interaction is not signficant so I'm refitting with a type 2
car::Anova(exp_balanus_NB1_2, type = "II") %>% 
  tidy() %>% 
  kableExtra::kable(caption = "p values for model fitted to Balanus field experiment data") %>% 
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# which sites were different?
grp_means_log <- emmeans(exp_balanus_NB1_2, specs = pairwise ~ site, data = exp_data)

pw <- as.data.frame(grp_means_log$contrasts)

pw %>%
  kableExtra::kable(caption = "tukey adjusted p values for pairwise comparisons from model fitted to Balanus field experiment data") %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left") %>%
  kableExtra::column_spec(1:6, color = "black", background = ifelse(pw$p.value < 0.05, "pink", "white"))

plot(grp_means_log, comparison = TRUE)

