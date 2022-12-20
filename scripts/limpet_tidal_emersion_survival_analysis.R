##### Survival analysis ##### 

############################### Lottia pelta  ################################## 
pelta_data <- read_csv("./data/raw/limpet_salinity_tolerance_tidal_pelta_exp.csv") %>% 
  clean_names()

head(pelta_data)

pelta_data <- pelta_data %>% 
  mutate(tide_treatment = factor(tide_treatment),
         salinity = factor(salinity, levels = c("10", "20", "30"))) %>% 
  filter(tank != "Table", !is.na(tank)) %>% 
  mutate(species = "Lottia_pelta") %>% 
  mutate(status = 1) %>% 
  rename(day = number_days_survived) %>% 
  select(species, tide_treatment, salinity, day, tank, status)

pelta_data$status[pelta_data$day > 28] <- 0

pelta_data$day[pelta_data$day > 28]  <- 28

# create a survival object to be fit  
surv_object <- Surv(time = pelta_data$day, event = pelta_data$status)

surv_diff <- survdiff(Surv(day, status) ~ salinity, data = pelta_data)

# create Kaplan-Meier survival curves 
pelta_fit <- survfit(surv_object ~ salinity + tide_treatment, data = pelta_data)

lottia_pelta_fig <- ggsurvplot(pelta_fit, data = pelta_data, pval = FALSE, 
                     facet.by = "salinity", 
                     nrow = 1, 
                     conf.int = TRUE,
                     legend.title = "Tidal treatment",
                     legend.labs = c("Subtidal", "Intertidal"),
                     xlab = "",
                     ylab = expression(paste(italic("Lottia pelta"),  " Survival probability")),
                     ggtheme = theme_classic(), 
                     palette = c("#5ab4ac","#d8b365"),
                     axis.title = element_text(size = 10, face = "bold", colour = "grey30"),
                     font.legend = list(size = 10, face = "bold", color = "grey30"))


######################## RMST for Lottia pelta ####################### 

mean_survival <- as_tibble(survival:::survmean(fit, rmean=28)[[1]])

mean_survival <- clean_names(mean_survival)

mean_survival$salinity <- c(10,10,20,20,30,30)

mean_survival$tide_treatment <- rep(c("In", "Out"), 3)
mean_survival$tide_treatment <- factor(mean_survival$tide_treatment, levels = c("In", "Out"))
cols <- c("#5ab4ac","#d8b365")

b <- ggplot(data = mean_survival, aes(x = salinity, y = rmean, colour = tide_treatment)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = rmean-se_rmean, ymax=rmean+se_rmean), width = 0.5) + 
  scale_color_manual(values = cols) + 
  scale_y_continuous(labels = c(5,10,15,20,25,30), breaks = c(5,10,15,20,25,30)) + 
  labs(y = "RMST", x = "Salinity (psu)") +  
  theme(axis.title = element_text(size = 12, colour = "grey30"), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        legend.key = element_blank(), 
        legend.title = element_text(size = 12, face = "bold", colour = "grey30"), 
        legend.position = "none",
        legend.text = element_text(size = 12, colour = "grey30"))


############################### Lottia digitalis  ################################## 
digitalis_data <- read_csv("./data/raw/limpet_salinity_tolerance_tidal_digitalis_exp.csv") %>% 
  clean_names()

head(digitalis_data)

digitalis_data <- digitalis_data %>% 
  mutate(tide_treatment = factor(tide_treatment),
         salinity = factor(salinity, levels = c("10", "20", "30"))) %>% 
  filter(tank != "No Tank", !is.na(tank)) %>% 
  mutate(species = "Lottia_digitalis") %>% 
  mutate(status = 1) %>% 
  rename(day = number_days_survived) %>% 
  select(species, tide_treatment, salinity, day, tank, status)

digitalis_data$status[digitalis_data$day > 28] <- 0

digitalis_data$day[digitalis_data$day > 28]  <- 28

# create a survival object to be fit  
surv_object <- Surv(time = digitalis_data$day, event = digitalis_data$status)

# create Kaplan-Meier survival curves 
digitalis_fit <- survfit(surv_object ~ salinity + tide_treatment, data = digitalis_data)

lottia_digitalis_fig <- ggsurvplot(digitalis_fit, data = digitalis_data, pval = FALSE, 
                               facet.by = "salinity", 
                               nrow = 1, 
                               conf.int = TRUE,
                               legend = "none",
                               legend.labs = c("Subtidal", "Intertidal"),
                               xlab = "Day",
                               ylab = expression(paste(italic("Lottia digitalis"),  " Survival probability")),
                               ggtheme = theme_classic(), 
                               palette = c("#5ab4ac","#d8b365"),
                               axis.title = element_text(size = 10, face = "bold", colour = "grey30"),
                               font.legend = list(size = 10, face = "bold", color = "grey30"))

final_fig <- lottia_pelta_fig + lottia_digitalis_fig + plot_layout(ncol = 1)

ggsave("./figures/limpet_tidal_emersion_survival_figure.jpg", plot = final_fig, height = 6, width = 8)
