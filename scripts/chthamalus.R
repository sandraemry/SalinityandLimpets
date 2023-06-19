### GLM of Chthamalus dalli for the field experiment 

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
         site = factor(site, levels =c("EC", "HS", "RP", "HB", "LB", "RS"))) %>% 
  select(region, site, treatment, fucus_pt, ulva_spp_pt, balanus_no, chthamalus_no, masto_crust_pt, diatom_pt)


# Fit models -----------------------------------------------------------------

hist(exp_data$chthamalus_no) # lots of zeros 

# fit models 
exp_chthamalus_poisson <- glmmTMB(round(chthamalus_no) ~ treatment * region +  (1|region/site), data = exp_data, family = poisson(link = "log"))

exp_chthamalus_NB2 <- update(exp_balanus_poisson, family = nbinom2) 

exp_chthamalus_NB1 <- update(exp_balanus_poisson, family = nbinom1) 

exp_chthamalus_tweedie <- update(exp_balanus_poisson, family = tweedie) 

# models with zero-inflation
exp_chthamalus_zipoisson <- glmmTMB(round(chthamalus_no) ~ treatment * region +  (1|region/site), 
                                    data = exp_data, 
                                    ziformula = ~1, family = poisson(link = "log"))

exp_chthamalus_ziNB2 <- update(exp_chthamalus_zipoisson, family = nbinom2) 

exp_chthamalus_ziNB1 <- update(exp_chthamalus_zipoisson, family = nbinom1) # best model 


# model selection 
AICtab(exp_chthamalus_poisson, exp_chthamalus_NB1, exp_chthamalus_NB2, exp_chthamalus_tweedie,
       exp_chthamalus_zipoisson, exp_chthamalus_ziNB2, exp_chthamalus_ziNB1)


#checking assumptions 
set.seed(4444)
exp_chthamalus_sim_res <- simulateResiduals(exp_chthamalus_ziNB1)

plot(exp_chthamalus_sim_res) # good

# Model outputs -----------------------------------------------------------

# parameters
coef <- summary(exp_chthamalus_ziNB1)$coefficients$cond

coef %>% 
  kableExtra::kable(caption = "coefficient estimates for model fitted to Chthamalus field experiment data") %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

car::Anova(exp_chthamalus_ziNB1, type = "III") %>% 
  tidy() %>% 
  kableExtra::kable(caption = "p values for model fitted to Chthamalus field experiment data") %>% 
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

exp_chthamalus_anova_table <- car::Anova(exp_chthamalus_ziNB1, type = "III") %>% 
  tidy()

grp_means_log <- emmeans(exp_chthamalus_ziNB1, specs = pairwise ~ treatment | region, data = exp_data)

pw <- as.data.frame(grp_means_log$contrasts)

pw %>%   
  kableExtra::kable(caption = "tukey adjusted p values for pairwise comparisons from model fitted to Chthamalus field experiment data") %>% 
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left") %>% 
  kableExtra::column_spec(1:7, color = "black", background = ifelse(pw$p.value < 0.05, "yellow", "white"))

plot(grp_means_log, comparison = TRUE)



