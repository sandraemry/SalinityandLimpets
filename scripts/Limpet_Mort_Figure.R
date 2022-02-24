library(tidyverse)
library(gridExtra)
library(ggpubr)

#Load Data
figdata<-read_csv("./data/tidy/mortality_expmt_sum.csv")
figdata$Salinity <- as.factor(figdata$Salinity)

# Change treatment to population
figdata$Treatment[figdata$Treatment == "Low"] <- "West Vancouver"
figdata$Treatment[figdata$Treatment == "High"] <- "Galiano Island"

figdata <- figdata %>% rename(site = Treatment)

#Subset Data
Treat5 <- subset(figdata, Salinity == 5)
Treat8 <- subset(figdata, Salinity == 8)
Treat11 <- subset(figdata, Salinity == 11)
Treat14 <- subset(figdata, Salinity == 14)

#Create individual plots
five <- ggplot(Treat5, aes(x=Day, y=Prop_Alive, shape = site, color = site)) +
  geom_line() +
  geom_point(size = 3) +
  scale_shape_manual(values=c(16, 17))+
  scale_color_manual(values=c('black','grey50')) +
  geom_errorbar(aes(ymin = Prop_Alive-Std_Error, ymax = Prop_Alive+Std_Error), width = 0.2,
                position = position_dodge(0.05)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlim(1,28) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0,1)) +
  labs(title="A) 5 psu") +
  theme(plot.title = element_text(hjust = 1)) +
  xlab("") +
  ylab("") +
  theme(legend.position = c(0.93, 0.6))

eight <- ggplot(Treat8, aes(x=Day, y=Prop_Alive, shape = site, color = site)) +
  geom_line() +
  geom_point(size = 3) +
  scale_shape_manual(values=c(16, 17))+
  scale_color_manual(values=c('black','grey50')) +
  geom_errorbar(aes(ymin = Prop_Alive-Std_Error, ymax = Prop_Alive+Std_Error), width = 0.2,
                position = position_dodge(0.05)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlim(1,28) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0,1)) +
  labs(title="B) 8 psu") +
  theme(plot.title = element_text(hjust = 1)) +
  xlab("") +
  ylab("") +
  theme(legend.position = "none")

eleven <- ggplot(Treat11, aes(x=Day, y=Prop_Alive, shape = site, color = site)) +
  geom_line() +
  geom_point(size = 3) +
  scale_shape_manual(values=c(16, 17))+
  scale_color_manual(values=c('black','grey50')) +
  geom_errorbar(aes(ymin = Prop_Alive-Std_Error, ymax = Prop_Alive+Std_Error), width = 0.2,
                position = position_dodge(0.05)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlim(1,28) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0,1)) +
  labs(title="C) 11 psu") +
  theme(plot.title = element_text(hjust = 1)) +
  xlab("") +
  ylab("") +
  theme(legend.position = "none")

fourteen <- ggplot(Treat14, aes(x=Day, y=Prop_Alive, shape = site, color = site)) +
  geom_line() +
  geom_point(size = 3) +
  scale_shape_manual(values=c(16, 17))+
  scale_color_manual(values=c('black','grey50')) +
  geom_errorbar(aes(ymin = Prop_Alive-Std_Error, ymax = Prop_Alive+Std_Error), width = 0.2,
                position = position_dodge(0.05)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlim(1,28) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0,1)) +
  labs(title="D) 14 psu") +
  theme(plot.title = element_text(hjust = 1)) +
  xlab("") +
  ylab("") +
  theme(legend.position = "none")

#Create Multi-panel Figure
figure <- grid.arrange(five, eight, eleven, fourteen, nrow = 4)

annotate_figure(figure,
                left = text_grob("Mean Proportion Alive", rot=90,vjust=1),
                bottom = text_grob("Day",vjust=-1))

ggsave("./figures/limpet_mortality_local_adaptation.png", width = 12.31, height = 8.71)
                