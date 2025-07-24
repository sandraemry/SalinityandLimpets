# April 25, 2025
library(tidyverse)
library(lme4)
library(car)
library(emmeans)
library(glmmTMB)

# Step 1. Model ulva from the transects 
df <- read_csv("./data/tidy/field_transect_tidy.csv")

head(df)

df$month[df$month == "September"] <- "August"

df1 <- df |> 
  select(month:quadrat_no, ulva_spp_pt) |> 
  mutate(region = factor(region, levels = c("Low", "High")),
         month = factor(month, levels = c("May", "June", "July", "August")))


moda <- lmer(ulva_spp_pt ~ region * month + 1|region/site, data = df1) # fit is singular 
modb <- lm(ulva_spp_pt ~ site * month, data = df1)
plot(modb)
ulva_tweedie <- glmmTMB(ulva_spp_pt ~ month * region +  (1|region/site), 
                            data = df1, family = tweedie(link = "log"))


# changing percent cover to proportion
# transforming 0s and 1s according to Smithson M, Verkuilen J (2006). "A Better Lemon Squeezer? 
# Maximum-Likelihood Regression with Beta-Distributed Dependent Variables." Psychological Methods, 11 (1), 54–71.

# (y * (n−1) + 0.5) / n where n is the sample size.
n <- 8

df1 <- df1 |> 
  mutate(ulva_transformed = ulva_spp_pt/100) |> 
  mutate(ulva_transformed = ((ulva_transformed * (n-1)) + 0.5) / n)

# fit models 
ulva_beta <- glmmTMB(ulva_transformed ~ month * region +  (1|region/site), 
                        data = df1, family = beta_family(link = "logit"))

#checking assumptions 
set.seed(19)
ulva_beta_res <- simulateResiduals(ulva_beta)
plot(ulva_beta_res) # not good

#checking assumptions 
set.seed(19)
ulva_tweedie_res <- simulateResiduals(ulva_tweedie)
plot(ulva_tweedie_res) # BETTER 

Anova(ulva_tweedie, type = 3)
emmeans(ulva_tweedie, ~region|month,type="response")

df1 |> 
  group_by(region) |> 
  summarise(avg_ulva = mean(ulva_spp_pt), se_ulva = sd(ulva_spp_pt)/sqrt(96)) 





