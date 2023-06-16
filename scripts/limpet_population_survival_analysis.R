## limpet mortality survival analysis
library(tidyverse)
library(survival)
library(survminer)
library(janitor)
library(cowplot)

raw_mort <- read_csv("./data/raw/limpet_mortality_local_adaptation.csv")

raw_mort <- raw_mort %>% 
  clean_names() %>% 
  select(site:tank) %>% 
  mutate(status = 1) %>% 
  rename(day = days_survived) %>% 
  arrange(salinity, tank, site) %>% 
  mutate(site = factor(site, levels = c("Low", "High"))) %>% 
  as.data.frame() %>% 
  mutate(salinity = factor(salinity, levels = c("5", "8", "11", "14", "17", "20")))

raw_mort$status[raw_mort$day == 50] <- 0

raw_mort$day[raw_mort$day == 50]  <- 28


# Kaplan-Meier curves -----------------------------------------------------

# create a survival object to be fit  
surv_object <- Surv(time = raw_mort$day, event = raw_mort$status)

# create Kaplan-Meier survival curves 
fit <- survfit(surv_object ~ salinity + site, data = raw_mort)


KMplot <- ggsurvplot(fit, data = raw_mort, pval = FALSE, 
                     facet.by = "salinity", 
                     nrow = 1, 
                     conf.int = TRUE,
                     legend.title = "Region",
                     legend.labs = c("Low salinity", "High salinity"),
                     xlab = "Day",
                     ggtheme = theme_classic(), 
                     palette = c('orange', 'steelblue'),
                     axis.title = element_text(size = 10, face = "bold", colour = "grey30"),
                     font.legend = list(size = 10, face = "bold", color = "grey30"))

ggsave("./figures/limpet_mortality_salinity.png", KMplot)


# RMST --------------------------------------------------------------------

### For 28 days

mean_survival <- as_tibble(survival:::survmean(fit, rmean=28)[[1]])

mean_survival <- clean_names(mean_survival)

mean_survival$salinity <- c(5,5,8,8,11,11,14,14,17,17,20,20)

mean_survival$site <- rep(c("Low", "High"), 6)
mean_survival$site <- factor(mean_survival$site, levels = c("Low", "High"))
cols <- c("orange", "steelblue")

b <- ggplot(data = mean_survival, aes(x = salinity, y = rmean, colour = site)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = rmean-se_rmean, ymax=rmean+se_rmean), width = 0.5) + 
  scale_color_manual(values = cols) + 
  scale_y_continuous(labels = c(5,10,15,20,25,30), breaks = c(5,10,15,20,25,30)) + 
  labs(y = "RMST (days)", x = "Salinity (psu)") +  
  theme(axis.title = element_text(size = 12, colour = "grey30"), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        legend.key = element_blank(), 
        legend.title = element_text(size = 12, face = "bold", colour = "grey30"), 
        legend.position = "none",
        legend.text = element_text(size = 12, colour = "grey30"))
  
# a + b

ggsave("./figure/RMST_28days.png", plot = b, height = 8, width = 12)

surv_plot_grid <- plot_grid(KMplot, b, labels = "auto", ncol = 1, rel_heights = c(1.5,1))

ggsave("./figures/figure4.jpg", plot = surv_plot_grid, height = 5.5, width = 8)




