library(tidyverse)
library(ggpmisc)
library(gridExtra)

#Load datasets
massdata <- read_csv("./data/tidy/ulva_mass_expmt.csv")
etrdata <- read_csv("./data/tidy/ulva_etrmax_expmt.csv")

my_theme <- theme(axis.title = element_text(size = 12), 
                  panel.background = element_blank(), 
                  panel.border = element_rect(fill = NA), 
                  panel.grid = element_blank(),
                  legend.key = element_blank(), 
                  legend.title = element_text(size = 12, face = "bold"), 
                  legend.position = "none",
                  legend.text = element_text(size = 12))

#Mass plot
mass <- ggplot(massdata, aes(x = Salinity, y = Growth)) +
  geom_point(color = "forestgreen", alpha = 0.7) +
  theme_classic() +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = 0) +
  ylim(-4, 4) +
  xlab("") +
  ylab("Change in Mass (g)") +
  labs(title="a") +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, color = "forestgreen") + 
  my_theme
  
# +
#   stat_poly_eq(formula = y ~ x + I(x^2), aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~")),
#                geom = "text",
#                label.x = 15, label.y = -2.5) +
#   stat_fit_glance(method = 'lm', method.args = list(formula = y ~ x + I(x^2)), geom='text',
#                   aes(label=paste("p = ", signif(..p.value.., digits=3),sep = "")),
#                   label.x = 15, label.y = -3)


#ETRmax plot
etr <- ggplot(etrdata, aes(x = Salinity, y = etrmax)) +
  geom_point(color = "forestgreen") +
  theme_classic() +
  theme(panel.grid = element_blank()) +
  xlab("Salinity (psu)") +
  ylab(expression("ETRmax ("~mu~"mol e"^-1~"m"^-2~"s"^-1~")")) + 
  labs(title="b") +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, color = "forestgreen") + 
  my_theme

# +
#   stat_poly_eq(formula = y ~ x + I(x^2), aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~")),
#                geom = "text",
#                label.x = 15, label.y = 26) +
#   stat_fit_glance(method = 'lm', method.args = list(formula = y ~ x + I(x^2)), geom='text',
#                   aes(label=paste("p = ", signif(..p.value.., digits=3),sep = "")),
#                   label.x = 15, label.y = 22)

#Create Multi-panel Figure
figure <- grid.arrange(mass, etr, nrow = 2)

ggsave("./figures/figure5.jpg", 
       width = 8, height = 6, plot = figure)

