#### Ordination on transect surveys
library(tidyverse)
library(skimr)
library(vegan)
library(here)
library(permute)

theme_set(theme_classic())


# Data prep  --------------------------------------------------------------

# Load in data
exp <- read_csv(here::here("data", "tidy", "field_exclusion_tidy.csv"))

# create unique row name indicators
exp <- exp %>% 
  unite(col = "site_rep_treatment", c("site", "replicate", "treatment"), sep = "_", remove = FALSE) %>% 
  mutate(treatment = recode_factor(treatment, C = "Control", E = "Exclusion"), 
         site = factor(site, levels = c("EC", "HS", "RP", "HB", "LB", "RS")),
         region = factor(region, levels = c("Low", "High")))

# getting rid of redundant variables 
exp_comm_data <- exp %>% 
  dplyr::select(balanus_no:fucus_pt)


# NMDS --------------------------------------------------------------------

# Fit NMDS solutions for a number of k
k_vec <- 1:10
stress <- numeric(length(k_vec))
exp_dij <- metaMDSdist(exp_comm_data, autotransform = TRUE, 
                       trace = FALSE, distance = "bray")

set.seed(25)
for(i in seq_along(k_vec)) {
  sol <- metaMDSiter(exp_dij, k = i,
                     trace = FALSE)
  stress[i] <- sol$stress
}

par(mfrow = c(1,1))
plot(k_vec, stress, type = "b", ylab = "Stress",
     xlab = "Dimensions")

# NMDS with a double wisconsin transformation

set.seed(1986)
nmds_exp <- metaMDS(exp_comm_data, 
                    k = 3, trymax = 100, trace = FALSE, 
                    autotransform = TRUE, distance = "bray")

nmds_exp

gof_exp <- goodness(nmds_exp)

stressplot(nmds_exp)
plot(nmds_exp, type = "t", main = "goodness of fit")
points(nmds_exp, display ="sites", cex = gof_exp*100)


# Plotting ----------------------------------------------------------------


#extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(nmds_exp)$sites)

#add columns to data frame 
data.scores$Site <-  exp$site
data.scores$Salinity <-  exp$region
data.scores$Treatment <-  exp$treatment

head(data.scores)

exp_nmds_plot <- ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(shape = Treatment, colour = Salinity), size = 3, alpha = 0.8) + 
  scale_colour_manual(values = c("orange", "steelblue"))  + 
  stat_ellipse(aes(color = Salinity, linetype = Treatment)) + 
  scale_linetype_manual(values = c("solid", "dashed")) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30"),
        legend.position = "right",
        legend.key.width = unit(2.5, "line")) + 
  labs(colour = "Salinity Region") + 
  guides(color = guide_legend(order=1, override.aes = list(linetype = 0)),
         linetype = guide_legend(order=2),
         shape = guide_legend(order = 2)) 

exp_nmds_plot

ggsave(filename = "figure6.jpg", plot = exp_nmds_plot, path = here::here("figures"), width = 16, height = 10, units = "cm")

# Permanova ---------------------------------------------------------------

exp_env_data <- exp %>%
  dplyr::select(site, region, treatment, replicate) %>% 
  dplyr::mutate(region = factor(region))

# restricting permutations scheme 
perm_site <- how(within = Within(type = "free"), 
                 plots = Plots(strata = exp_env_data$site, type = "free"), 
                 blocks = NULL,
                 nperm = 999) 

set.seed(11)


exp_perma_site <- adonis2(exp_comm_data ~ region * treatment, 
                           data = exp_env_data, 
                           permutations = perm_site, 
                           method = "bray")

exp_perma_site

df <- data.frame(treatments = c("salinity region", "treatment", "salinity region x treatment", "residuals", "total"), df = exp_perma_site$aov.tab$Df, SS = exp_perma_site$aov.tab$SumsOfSqs, MS = exp_perma_site$aov.tab$MeanSqs, `pseudo F` = exp_perma_site$aov.tab$F.Model, R2 = exp_perma_site$aov.tab$R2, `p value` = exp_perma_site$aov.tab$`Pr(>F)`)

df[ , 3:6] <- round(df[, 3:6], 2)

write_csv(df, "/Users/sandraemry/Documents/coyle_paper/results/perma_exp.csv",
          col_names = TRUE)


# BETADISP test -----------------------------------------------------------


# standardize data with wisconsin method, then calculate a matrix of Bray-Curtis distances
dist <- vegdist(wisconsin(exp_comm_data), method = "bray")

# BETADISP test for salinity region
dispersion_salinity <- betadisper(dist, group = exp_env_data$region)
exp_disp_salinity <- permutest(dispersion_salinity) # greater dispersion in high salinity region

exp_disp_salinity_df <- data.frame(Df = exp_disp_salinity$tab$Df, 
                                   SS = exp_disp_salinity$tab$`Sum Sq`,
                                   MS = exp_disp_salinity$tab$`Mean Sq`, 
                                   `F` = exp_disp_salinity$tab$`F`,
                                   N.Perm = exp_disp_salinity$tab$N.Perm,
                                   `Pr(>F)` = exp_disp_salinity$tab$`Pr(>F)`) %>% 
  rename(`Pr(>F)` = `Pr..F.`) %>% 
  mutate(across(where(is.numeric), round, 3))

exp_disp_salinity_df[is.na(exp_disp_salinity_df)] <- ""

write_csv(exp_disp_salinity_df, here::here("results", "exp_disp_treatment.csv"))

# BETADISP test for treatment groups 
dispersion_treatment <- betadisper(dist, group=exp_env_data$treatment)
exp_disp_treatment <- permutest(dispersion_treatment) # greater dispersion in the control

exp_disp_treatment_df <- data.frame(Df = exp_disp_treatment$tab$Df, 
                                    SS = exp_disp_treatment$tab$`Sum Sq`,
                                    MS = exp_disp_treatment$tab$`Mean Sq`, 
                                    `F` = exp_disp_treatment$tab$`F`,
                                    N.Perm = exp_disp_treatment$tab$N.Perm,
                                    `Pr(>F)` = exp_disp_treatment$tab$`Pr(>F)`) %>% 
  rename(`Pr(>F)` = `Pr..F.`) %>% 
  mutate(across(where(is.numeric), round, 3))

exp_disp_treatment_df[is.na(exp_disp_treatment_df)] <- ""

write_csv(exp_disp_treatment_df, here::here("results", "exp_disp_salnity.csv"))


exp_disp <- rbind(exp_disp_salinity_df, exp_disp_treatment_df)

exp_disp %>% 
  kableExtra::kable(align = "l") %>% 
  kableExtra::kable_classic() %>% 
  kableExtra::kable_styling(full_width = F,
                            position = "left") %>% 
  kableExtra::pack_rows("dispersion by salinity region", 1, 2) %>%
  kableExtra::pack_rows("dispersion by treatment", 3, 4) %>% 
  kableExtra::save_kable("./figures/exp_betadisper.png")

# plots of tests
png("exp_betadisper_plot_A.png")
plot(dispersion_salinity, hull = FALSE, ellipse = TRUE, main = "", 
     col = c("orange", "steelblue")) # sd ellipse
title("A", adj = 0)
dev.off()

png("exp_betadisper_plot_B.png")
plot(dispersion_treatment, hull = FALSE, ellipse = TRUE, main="", 
     col = c("seagreen", "salmon4")) # sd ellipse
title("B", adj = 0)
dev.off()





