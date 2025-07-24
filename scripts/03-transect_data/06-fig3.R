library(tidyverse)
library(patchwork)

theme_set(theme_classic())


# # Step 1. Ulva plot -----------------------------------------------------


df <- read_csv("./data/tidy/field_transect_tidy.csv")

head(df)

df$site[df$site == "LB"] <- "LS1"
df$site[df$site == "HB"] <- "LS2"
df$site[df$site == "RS"] <- "LS3"
df$site[df$site == "HS"] <- "HS1"
df$site[df$site == "RP"] <- "HS2"
df$site[df$site == "EC"] <- "HS3"

df$month[df$month == "September"] <- "August"

df1 <- df |> 
  select(month:quadrat_no, ulva_spp_pt) |> 
  mutate(region = factor(region, levels = c("Low", "High")),
         month = factor(month, levels = c("May", "June", "July", "August")),
         site = factor(site, levels = c("LS1", "LS2", "LS3", "HS1", "HS2", "HS3")))

a <- ggplot(data = df1) + 
  geom_boxplot(aes(x = month, y = ulva_spp_pt, color = site)) + 
  scale_colour_manual(values = c("orange", "orange", "orange", "steelblue", "steelblue", "steelblue"))  + 
  labs(x = "", y = expression(paste(italic("Ulva"), " sp. (% cover)")),
       color = "Sites")


# # Step 2. Limpet plot ---------------------------------------------------


# read in data set for field transects
df <- read_csv("./data/tidy/field_transect_tidy.csv")

df$site[df$site == "LB"] <- "LS1"
df$site[df$site == "HB"] <- "LS2"
df$site[df$site == "RS"] <- "LS3"
df$site[df$site == "HS"] <- "HS1"
df$site[df$site == "RP"] <- "HS2"
df$site[df$site == "EC"] <- "HS3"

df2 <- df |> 
  mutate(total_limpets = digitalis_no + persona_no + scutum_no + 
           pelta_no + unknown_limpet_no + paradigitalis_no) |> 
  dplyr::select(month, site, region, quadrat_no, total_limpets) |> 
  mutate(month = factor(month, levels = c("May", "June", "July", "September")),
         region = factor(region, levels =c("Low", "High")),
         site = factor(site, levels = c("LS1", "LS2", "LS3", "HS1", "HS2", "HS3")))

b <- ggplot(data = df2, aes(x = month, y = log(total_limpets + 1), color = site)) + 
  geom_boxplot() + 
  scale_colour_manual(values = c("orange", "orange", "orange", "steelblue", "steelblue", "steelblue"))  + 
  labs(x = "Month", y = "Log of Limpets (no. + 1)") + 
  theme(legend.position="none")


fig3 <- a / b
ggsave("./figures/fig3-ulva_limpet_transects.jpg", plot = fig3)


