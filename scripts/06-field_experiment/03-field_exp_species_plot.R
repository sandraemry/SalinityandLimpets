# two panel plot of ulva + chthamalus
# 6 panel plot for SM
library(tidyverse)
library(patchwork)
library(here)

my_theme <- theme(axis.title = element_text(size = 12), 
                    panel.background = element_blank(), 
                    panel.border = element_rect(fill = NA), 
                    panel.grid = element_blank(),
                    legend.key = element_blank(), 
                    legend.title = element_text(size = 12, face = "bold"), 
                    legend.position = "none",
                    legend.text = element_text(size = 12))

exp_data <- read_csv(here::here("data", "tidy", "field_exclusion_tidy.csv"))

exp_data <- exp_data %>% 
  mutate(treatment = factor(treatment, levels = c("control", "exclusion")),
         region = factor(region, levels = c("Low", "High")),
         site = factor(site, levels = c("LB", "RS", "HB", "RP", "EC", "HS"))) %>% 
  select(region, site, treatment, fucus_pt, ulva_spp_pt, balanus_no, chthamalus_no, mytilus_pt, amphipod_no, masto_crust_pt, diatom_pt, pyropia_pt)

cols = c("Low" = "orange", "High" = "steelblue")

# a <- ggplot(data = exp_data) + geom_boxplot(aes(x = treatment, y = chthamalus_no, fill = region)) + 
#   scale_fill_manual(values = cols) + 
#   labs(x = "herbivore treatment", y = expression(paste(italic("Chthamalus dalli")))) + 
#   my_theme

      
# cols <- c("RP" = "#a6bddb", "EC" = "#74a9cf", "HS" = "#2b8cbe", "HB" = "#fec44f", "LB" = "#fe9929", "RS" = "#fee391")
# levels(exp_data$site)[levels(exp_data$site)=="Ruckle"] <- "RP"
# levels(exp_data$site)[levels(exp_data$site)=="Eagle Cove"] <- "EC"
# levels(exp_data$site)[levels(exp_data$site)=="Hailstorm 2"] <- "HS"
# levels(exp_data$site)[levels(exp_data$site)=="Horseshoe Bay"] <- "HB"
# levels(exp_data$site)[levels(exp_data$site)=="Lions Bay 2"] <- "LB"
# levels(exp_data$site)[levels(exp_data$site)=="Rope Site 2"] <- "RS"
# 
# b <- ggplot(data = exp_data) + geom_boxplot(aes(x = treatment, y = balanus_no, fill = site)) + 
#   scale_fill_manual(values = cols) + 
#   labs(x = "herbivore treatment", y = expression(paste(italic("Balanus glandula"), " abundance"))) + 
#   theme(axis.title = element_text(size = 11, face = "bold", colour = "grey30"), 
#         legend.position = "right",
#         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"))

# b <- ggplot(data = exp_data) + geom_boxplot(aes(x = site, y = balanus_no, fill = treatment)) + 
#   # scale_fill_manual(values = cols) + 
#   labs(x = "herbivore treatment", y = expression(paste(italic("Balanus glandula"), " abundance"))) + 
#   theme(axis.title = element_text(size = 11, face = "bold", colour = "grey30"), 
#         legend.position = "right",
#         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"))

# b <- ggplot(data = exp_data) + geom_boxplot(aes(x = treatment, y = ulva_pt, fill = region)) + 
#   scale_fill_manual(values = cols) + 
#   labs(x = "herbivore treatment", y = expression(paste(italic("Ulva"), " sp. (% cover)")), fill = "salinity region") + 
#   theme(axis.title = element_text(size = 12, face = "bold", colour = "grey30"), 
#       panel.background = element_blank(), 
#       panel.border = element_rect(fill = NA, colour = "grey30"), 
#       legend.key = element_blank(), 
#       legend.title = element_text(size = 12, face = "bold", colour = "grey30"), 
#       legend.position = "right",
#       legend.text = element_text(size = 12, colour = "grey30"))
# 
# 
# species_abundance_plot <- a + b 
# 
# ggsave("./figures/species_abundance_plot.png", species_abundance_plot, height = 4.92, width = 7)  
  

# Means + Error Plot ------------------------------------------------------

# n <- 7
# 
# summary_data <- exp_data %>% 
#   group_by(region, treatment) %>% 
#   summarise(mean_ulva = mean(ulva_pt), se_ulva = sd(ulva_pt)/sqrt(n),
#             mean_chthamalus = mean(chthamalus_no), se_chthamalus = sd(chthamalus_no)/sqrt(n)) %>% 
#   ungroup()

std_mean <- function(x) sd(x)/sqrt(7)

summary_data <- exp_data %>% 
  group_by(region, treatment) %>% 
  summarise(across(contains("_"), list(mean = mean, se = std_mean), .names = "{.fn}_{.col}"))

c <- ggplot(data = summary_data, aes(x = treatment, y = mean_ulva_spp_pt, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_ulva_spp_pt - se_ulva_spp_pt, ymax = mean_ulva_spp_pt + se_ulva_spp_pt), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = expression(paste(italic("Ulva"), " sp. (% cover)")), 
       color = "Salinity region", title = "a") + 
  theme(axis.title = element_text(size = 12), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA), 
        panel.grid = element_blank(),
        legend.key = element_blank(), 
        legend.position = "right",
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 12)) + 
  guides(color = guide_legend(override.aes = list(linetype = 0, size = 2)))
  
  
d <- ggplot(data = summary_data, aes(x = treatment, y = mean_chthamalus_no, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_chthamalus_no - se_chthamalus_no, ymax = mean_chthamalus_no + se_chthamalus_no), width = .1, position = position_dodge(0.3)) + 
  labs(x = "Herbivore treatment", y = expression(paste(italic("Chthamalus dalli"), " (no.)")), title = "b") + 
  my_theme


means_errors_main <- (c / d)

ggsave("./figures/means_errors_ulva_chthamalus.jpg", 
       plot = means_errors_main)

ggsave("./figures/figure7.jpg", 
       width = 8, height = 6.5,
       plot = means_errors_main)

############################## Other species plots ##############################

e <- ggplot(data = summary_data, aes(x = treatment, y = mean_balanus_no, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_balanus_no - se_balanus_no, ymax = mean_balanus_no + se_balanus_no), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = expression(paste(italic("Balanus glandula"), " (no.)")), 
       fill = "Salinity region", title = "A") + 
  my_theme + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank())

f <- ggplot(data = summary_data, aes(x = treatment, y = mean_mytilus_pt, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_mytilus_pt - se_mytilus_pt, ymax = mean_mytilus_pt + se_mytilus_pt), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = expression(paste(italic("Mytilus trossulus"), , " (no.)")), 
       fill = "Salinity region", , title = "B") + 
  my_theme + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank())

g <- ggplot(data = summary_data, aes(x = treatment, y = mean_diatom_pt, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_diatom_pt - se_diatom_pt, ymax = mean_diatom_pt + se_diatom_pt), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = "Diatoms (% cover)", colour = "Salinity region", title = "C") + 
  theme(axis.title = element_text(size = 12), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA), 
        panel.grid = element_blank(),
        legend.key = element_blank(), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.position = "right",
        legend.text = element_text(size = 12)) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank())

h <- ggplot(data = summary_data, aes(x = treatment, y = mean_masto_crust_pt, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_masto_crust_pt - se_masto_crust_pt, ymax = mean_masto_crust_pt + se_masto_crust_pt), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = expression(paste(italic("Petrocelis"), " (% cover)")), 
       fill = "Salinity region", title = "D") + 
  my_theme

i <- ggplot(data = summary_data, aes(x = treatment, y = mean_pyropia_pt, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_pyropia_pt - se_pyropia_pt, ymax = mean_pyropia_pt + se_pyropia_pt), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = expression(paste(italic("Pyropia"), " sp. (% cover)")), 
       fill = "Salinity region", title = "E") + 
  theme(axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 14),
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA), 
        panel.grid = element_blank(),
        legend.key = element_blank(), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = 12))

j <- ggplot(data = summary_data, aes(x = treatment, y = mean_fucus_pt, colour = region)) + 
  geom_point(position = position_dodge(0.3)) + 
  scale_colour_manual(values = cols) + 
  geom_errorbar(aes(ymin = mean_fucus_pt - se_fucus_pt, ymax = mean_fucus_pt + se_fucus_pt), width = .1, position = position_dodge(0.3)) + 
  labs(x = "", y = expression(paste(italic("Fucus distichus"), " (% cover)")), 
       fill = "Salinity region", title = "F") + 
  my_theme

xlab <- "Herbivore treatment"

multi_species_plot_SM <- e + f + g + h + i + j + grid.draw(grid::textGrob(xlab, x = 0.45, y = 0.025, gp = gpar(col = "black", fontsize = 12)))
ggsave("./figures/multi_species_plot_SM.png", plot = multi_species_plot_SM)

k <- ggplot(data.frame(l = "Herbivore treatment", x = 1, y = 1)) +
  geom_text(aes(x, y, label = l)) + 
  theme_void() +
  coord_cartesian(clip = "off")

(e + f + g + h + i + j)/k

grid.arrange(patchworkGrob(e + f + g + h + i + j), bottom = "Herbivore treatment")

# p4 + (p1 / (p2 | p3)) + plot_layout(widths = c(1, 25))
ggsave("./figures/multi_species_plot_SM.png", plot = multi_species_plot_SM, height = 4.9, width = 6.9)

  

