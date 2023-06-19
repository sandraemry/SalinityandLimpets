
## Plots for SM of M. trossulus, B. glandula, F. distichus, C. dalli, Petrocelis, L. paradigitalis, 
## L. pelta, barnacle recruits 

library(tidyverse)
library(patchwork)

my_theme <- theme(axis.title = element_text(size = 12, colour = "grey30"), 
                  panel.background = element_blank(), 
                  panel.border = element_rect(fill = NA, colour = "grey30"), 
                  panel.grid = element_blank(),
                  legend.key = element_blank(), 
                  legend.title = element_text(size = 12, face = "bold", colour = "grey30"), 
                  legend.position = "none",
                  legend.text = element_text(size = 12, colour = "grey30"))

# read in data set for field transects
transect <- read_csv(here::here("data", "tidy", "field_transect_tidy.csv"))

#creating a unique name for each transect to be used as row names 
transect <- transect %>% 
  mutate(site = factor(site, levels = c("EC", "HS", "RP", "HB", "LB", "RS")),
         month = factor(month, levels = c("May", "June", "July", "September")),
         region = factor(region, levels = c("Low", "High"))) %>% 
  mutate(month = recode_factor(month, May = "May", June = "June", July = "July", September = "August")) %>% 
  select(site, month, region, mytilus_pt, balanus_pt, chthamalus_pt, fucus_pt, mastocarpus_crust_pt, 
         paradigitalis_no, pelta_no, barnacle_recruits_pt)

cols = c("Low" = "orange", "High" = "steelblue")

std_mean <- function(x) sd(x)/sqrt(8)

summary_data <- transect %>% 
  group_by(region, month) %>% 
  summarise(across(contains("_"), list(mean = mean, se = std_mean), .names = "{.fn}_{.col}"))

a <- ggplot(data = summary_data, aes(x = month, y = mean_mytilus_pt, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_mytilus_pt - se_mytilus_pt, ymax = mean_mytilus_pt + se_mytilus_pt), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = expression(paste(italic("M. trossulus"), " (% cover)")), title = "a") + 
  my_theme + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

b <- ggplot(data = summary_data, aes(x = month, y = mean_balanus_pt, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_balanus_pt - se_balanus_pt, ymax = mean_balanus_pt + se_balanus_pt), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = expression(paste(italic("B. glandula"), " (% cover)")), title = "b") + 
  my_theme + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

c <- ggplot(data = summary_data, aes(x = month, y = mean_chthamalus_pt, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_chthamalus_pt - se_chthamalus_pt, ymax = mean_chthamalus_pt + se_chthamalus_pt), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = expression(paste(italic("C. dalli"), " (% cover)")), title = "c") + 
  my_theme + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

d <- ggplot(data = summary_data, aes(x = month, y = mean_fucus_pt, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_fucus_pt - se_fucus_pt, ymax = mean_fucus_pt + se_fucus_pt), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = expression(paste(italic("F. distichus"), " (% cover)")), title = "d") + 
  my_theme + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

e <- ggplot(data = summary_data, aes(x = month, y = mean_mastocarpus_crust_pt, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_mastocarpus_crust_pt - se_mastocarpus_crust_pt, ymax = mean_mastocarpus_crust_pt + se_mastocarpus_crust_pt), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = expression(paste(italic("Petrocelis"), " (% cover)")), title = "e") + 
  my_theme 

f <- ggplot(data = summary_data, aes(x = month, y = mean_paradigitalis_no, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_paradigitalis_no - se_paradigitalis_no, ymax = mean_paradigitalis_no + se_paradigitalis_no), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = expression(paste(italic(" L. paradigitalis"), " (no.)")), title = "f") + 
  my_theme 

g <- ggplot(data = summary_data, aes(x = month, y = mean_pelta_no, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_pelta_no - se_pelta_no, ymax = mean_pelta_no + se_pelta_no), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = expression(paste(italic("L. pelta"), " (no.)")), title = "g") + 
  my_theme 

h <- ggplot(data = summary_data, aes(x = month, y = mean_barnacle_recruits_pt, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_barnacle_recruits_pt - se_barnacle_recruits_pt, ymax = mean_barnacle_recruits_pt + se_barnacle_recruits_pt), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = "Barnacle recruits (% cover)", fill = "salinity region", title = "h") + 
  my_theme 

i <- ggplot(data.frame(l = "month", x = 5, y = 5)) +
  geom_text(aes(x, y, label = l), angle = 0) + 
  theme_void() +
  coord_cartesian(clip = "off")

ylab <- "Month"
transectmulti_species_plot_SM <- 
a + b + c + d + e + f + g + h +  plot_layout(ncol = 4) 
grid::grid.draw(grid::textGrob(ylab, x = 0.5, y = 0.025, hjust = "centre", vjust= "top", rot = 0))
grid::grid.text(fontsize = 10)

ggsave(plot = transectmulti_species_plot_SM, here("figures", "transect_multispecies_plot_SM.png"))

