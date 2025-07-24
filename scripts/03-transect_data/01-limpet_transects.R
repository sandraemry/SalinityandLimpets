# Modelling limpet densities in transect data 
library(tidyverse)
library(lme4)
library(car)
library(broom)
library(emmeans)


# Data prep ---------------------------------------------------------------

# read in data set for field transects
transect <- read_csv("./data/tidy/field_transect_tidy.csv")

head(transect)
names(transect)
transect$site[transect$site == "LB"] <- "LS1"
transect$site[transect$site == "HB"] <- "LS2"
transect$site[transect$site == "RS"] <- "LS3"
transect$site[transect$site == "HS"] <- "HS1"
transect$site[transect$site == "RP"] <- "HS2"
transect$site[transect$site == "EC"] <- "HS3"

transect_limpets <- transect |> 
  mutate(total_limpets = digitalis_no + persona_no + scutum_no + 
           pelta_no + unknown_limpet_no + paradigitalis_no) |> 
  dplyr::select(month, site, region, quadrat_no, total_limpets) |> 
  mutate(month = factor(month, levels = c("May", "June", "July", "September")),
         region = factor(region, levels =c("Low", "High")),
         site = factor(site, levels = c("LS1", "LS2", "LS3", "HS1", "HS2", "HS3")))

limpet_moda <- lmer(total_limpets ~ month * region + (1|site), data = transect_limpets)
limpet_modb <- glmer(total_limpets ~ month * region + (1|site), 
                     family = poisson(link = "log"), data = transect_limpets)
limpet_modc <- glm.nb(total_limpets ~ month * site, 
                    data = transect_limpets)

plot(limpet_modc)
summary(limpet_modc)
car::Anova(limpet_modc, type = 3)

transect_limpets |> 
  group_by(region) |> 
  summarise(avg_limpet = mean(total_limpets), se_limpets = sd(total_limpets)/sqrt(96)) 
  
