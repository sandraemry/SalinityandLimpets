library(tidyverse)
library(ggpmisc)
library(gridExtra)

#Load datasets
massdata <- read_csv("ulva_mass_expmt.csv")
etrdata <- read_csv("ulva_etrmax_expmt.csv")

#Mass plot
mass <- ggplot(massdata, aes(x = Salinity, y = Growth)) +
  geom_point() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = 0) +
  ylim(-4, 4) +
  xlab("Salinity (ppt)") +
  ylab("Change in Mass (g)") +
  labs(title="A) Mass") +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, color = "black") +
  stat_poly_eq(formula = y ~ x + I(x^2), aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~")),
               geom = "text",
               label.x = 15, label.y = -2.5) +
  stat_fit_glance(method = 'lm', method.args = list(formula = y ~ x + I(x^2)), geom='text',
                  aes(label=paste("p = ", signif(..p.value.., digits=3),sep = "")),
                  label.x = 15, label.y = -3)


#ETRmax plot
etr <- ggplot(etrdata, aes(x = Salinity, y = etrmax)) +
  geom_point() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlab("Salinity (ppt)") +
  ylab("ETRmax") +
  labs(title="B) ETRmax") +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, color = "black") +
  stat_poly_eq(formula = y ~ x + I(x^2), aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~")),
               geom = "text",
               label.x = 15, label.y = 26) +
  stat_fit_glance(method = 'lm', method.args = list(formula = y ~ x + I(x^2)), geom='text',
                  aes(label=paste("p = ", signif(..p.value.., digits=3),sep = "")),
                  label.x = 15, label.y = 22)

#Create Multi-panel Figure
figure <- grid.arrange(mass, etr, nrow = 1)

