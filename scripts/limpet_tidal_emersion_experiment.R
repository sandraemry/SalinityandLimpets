library(tidyverse)
library(janitor)
library(lme4)
library(car)

############################# Lottia pelta #############################
# (this species name is from the Mortality Expmt 2-metadata word doc)
data <- read_csv("./data/raw/limpet_salinity_tolerance_tidal_pelta_exp.csv") %>% 
  clean_names()

head(data)

pelta_data <- data %>% 
  mutate(tide_treatment = factor(tide_treatment)) %>% 
  filter(tank != "Table", !is.na(tank)) %>% 
  mutate(species = "Lottia_pelta")

mod <- lmer(number_days_survived ~ tide_treatment * salinity + (1|tank), 
            data = pelta_data)

plot(mod)

Anova(mod, type = 3)

cols = c("In" = "#5ab4ac", "Out" = "#d8b365")

ggplot(data = pelta_data) + 
  geom_boxplot(aes(x = salinity, y = number_days_survived, fill = tide_treatment)) + 
  scale_fill_manual(values = cols) + 
  labs(x = "Salinity (psu)", y = "Number of survived", fill = "tide treatment") + 
  theme(axis.title = element_text(size = 12, colour = "grey30"), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        panel.grid = element_blank(),
        legend.key = element_blank(), 
        legend.title = element_text(size = 12, face = "bold", colour = "grey30"), 
        legend.position = "right",
        legend.text = element_text(size = 12, colour = "grey30"))

############################# Lottia pelta - binomial response ############################# 
# change to a binomial column of survived = 1, died = 0

pelta_data <- pelta_data %>% 
  mutate(survived = case_when(number_days_survived > 28 ~ 1,
                              number_days_survived <= 28 ~ 0)) 

binomial_mod_pelta_1 <- glmmTMB(survived ~ tide_treatment * salinity  + (1|tank),
                                    data = pelta_data, 
                                    family = binomial(link = "logit"))

mod1_res <- simulateResiduals(binomial_mod_pelta_1)
plot(mod1_res)
summary(binomial_mod_pelta_1)
Anova(binomial_mod_pelta_1)
ggpredict(binomial_mod_pelta_1, terms = c("salinity", "tide_treatment"))

pelta_binomial_fig <- ggplot(data = pelta_data, aes(x = salinity, y = survived, color = tide_treatment)) + 
  geom_point() + 
  stat_smooth(method = "glm", aes(color = tide_treatment), se = TRUE, 
              method.args = list(family = binomial)) + 
  scale_color_manual(values = cols) + 
  labs(x = "Salinity (psu)", y = "Proportion survived", 
       fill = "tidal treatment", subtitle = "Lottia pelta") + 
  theme(axis.title = element_text(size = 12, colour = "grey30"), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        panel.grid = element_blank(),
        legend.key = element_blank(), 
        legend.title = element_text(size = 12, face = "bold", colour = "grey30"), 
        legend.position = "right",
        legend.text = element_text(size = 12, colour = "grey30"))


############################## Lottia digitalis ####################################
# (this species name is from the Mortality exp1-metadata word doc)

data <- read_csv("./data/raw/limpet_salinity_tolerance_tidal_digitalis_exp.csv") %>% 
  clean_names()

head(data)

digitalis_data <- data %>% 
  mutate(tide_treatment = factor(tide_treatment)) %>% 
  filter(tank != "No Tank") %>% 
  mutate(species = "Lottia_digitalis")

# I'm assuming the number_days_survived = 50 are ones that didn't die? 
# digitalis_data$number_days_survived[digitalis_data$number_days_survived > 28] <- 29

mod <- lmer(number_days_survived ~ tide_treatment * salinity + (1|tank), 
            data = digitalis_data)

Anova(mod, type = 3)

ggplot(data = digitalis_data) + 
  geom_boxplot(aes(x = salinity, y = number_days_survived, fill = tide_treatment)) + 
  scale_fill_manual(values = cols) + 
  labs(x = "Salinity (psu)", y = "Number of Lottia digitalis survived", fill = "tide treatment") + 
  theme(axis.title = element_text(size = 12, colour = "grey30"), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        panel.grid = element_blank(),
        legend.key = element_blank(), 
        legend.title = element_text(size = 12, face = "bold", colour = "grey30"), 
        legend.position = "right",
        legend.text = element_text(size = 12, colour = "grey30"))

######################### Lottia digitalis - binomial response ######################### 
 # change to a binomial column of survived = 1, died = 0

digitalis_data <- digitalis_data %>% 
  mutate(survived = case_when(number_days_survived > 28 ~ 1,
                              number_days_survived <= 28 ~ 0)) 

binomial_mod_digitalis_1 <- glmmTMB(survived ~ tide_treatment * salinity  + (1|tank),
                                  data = digitalis_data, 
                                  family = binomial(link = "logit"))

mod1_res <- simulateResiduals(binomial_mod_digitalis_1)
plot(mod1_res)
summary(binomial_mod_digitalis_1)
Anova(binomial_mod_digitalis_1)
ggpredict(binomial_mod_digitalis, terms = c("salinity", "tide_treatment"))

digitalis_binomial_fig <- ggplot(data = digitalis_data, aes(x = salinity, y = survived, color = tide_treatment)) + 
  geom_point(position = position_jitter(w = 0.4, h = 0), alpha = 0.5) + 
  stat_smooth(method = "glm", aes(color = tide_treatment), se = TRUE, 
              method.args = list(family = binomial)) + 
  scale_color_manual(values = cols) + 
  labs(x = "Salinity (psu)", y = "Proportion survived", 
       fill = "tidal treatment", subtitle = "Lottia digitalis") + 
  theme(axis.title = element_text(size = 12, colour = "grey30"), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        panel.grid = element_blank(),
        legend.key = element_blank(), 
        legend.title = element_text(size = 12, face = "bold", colour = "grey30"), 
        legend.position = "right",
        legend.text = element_text(size = 12, colour = "grey30"))

# digitalis_sum_data <- digitalis_data %>% 
#   group_by(tank, tide_treatment) %>% 
#   mutate(total_survived = sum(survived),
#             proportion_survived = total_survived/4) %>% 
#   select(-c(number_days_survived, size_mm, survived, total_survived))
# 
# digitalis_sum_data <- digitalis_sum_data[!duplicated(digitalis_sum_data), ]
# binomial_mod_digitalis_2 <- glmmTMB(proportion_survived ~ tide_treatment * salinity  + (1|tank),
#                                   data = digitalis_sum_data, 
#                                   family = binomial(link = "logit"))
  
