#### Ordination on transect surveys
library(tidyverse)
library(skimr)
library(vegan)
library(here)
library(permute)

theme_set(theme_classic())


# Data prep ---------------------------------------------------------------

# read in data set for field transects
transect <- read_csv(here::here("data", "tidy", "field_transect_tidy.csv"))

#creating a unique name for each transect to be used as row names 
transect <- transect %>% 
  unite(col = "site_rep_month", c("site", "quadrat_no", "month"), sep = "_", remove = FALSE) %>% 
  mutate(site = factor(site, levels = c("EC", "HS", "RP", "HB", "LB", "RS")),
         month = factor(month, levels = c("May", "June", "July", "September")),
         region = factor(region, levels = c("Low", "High")))

# parse out only the species data, and getting rid of redundant variables  
survey_comm_data <- transect %>% 
  select(balanus_pt:ulva_spp_pt) 

# recoding factors 
survey_env_data <- transect %>%
  select(month, site, region) %>% 
  mutate(month = recode_factor(month, May = "May", June = "June", July = "July", September = "August"))


# NMDS --------------------------------------------------------------------


# Fit NMDS solutions for a number of k
k_vec <- 1:10
stress <- numeric(length(k_vec))
survey_dij <- metaMDSdist(survey_comm_data, autotransform = TRUE, trace = FALSE, distance = "bray")
set.seed(25)

for(i in seq_along(k_vec)) {
  sol <- metaMDSiter(survey_dij, k = i,
                     trace = FALSE)
  stress[i] <- sol$stress
}

plot(k_vec, stress, type = "b", ylab = "Stress",
     xlab = "Dimensions")

#  NMDS with 3 dimensions with a double Wisconsin transformation

set.seed(31)
nmds_survey <- metaMDS(survey_comm_data, k = 3, trymax = 100, trace = FALSE, autotransform = TRUE, distance = "bray")

nmds_survey

# Stress and goodness of fit plots
# Large circles around sites indicate that the model is not fitting those sites very well.  
gof <- goodness(nmds_survey)

stressplot(nmds_survey)
plot(nmds_survey, type="t", main = "goodness of fit")
points(nmds_survey, display="sites", cex=gof*100)

#extract NMDS scores (x and y coordinates)
data.scores <- as.data.frame(scores(nmds_survey)$sites)

#add columns to data frame 
data.scores$Site <- survey_env_data$site
data.scores$Month <- survey_env_data$month
data.scores$Salinity <- survey_env_data$region

survey_nmds_plot <- ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(colour = Salinity), size = 3, alpha = 0.8) + 
  scale_colour_manual(values = c("orange", "steelblue"))  + 
  stat_ellipse(aes(colour = Salinity, linetype = Month)) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotdash", "dotted")) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30"),
        legend.position = "right") + 
  labs(colour = "Salinity Region") + 
  guides(colour = guide_legend(order=1, override.aes = list(linetype = 0)),
         linetype = guide_legend(order=2))

survey_nmds_plot

ggsave(filename = "survey_nmds.jpg", plot = survey_nmds_plot, 
       path = here::here("figures"), 
       width = 16, height = 10, 
       units = "cm")

ggsave(filename = "figure3.jpg", plot = survey_nmds_plot, 
       path = here::here("figures"), 
       width = 16, height = 10, 
       units = "cm")


# Permanova ---------------------------------------------------------------

# permutation scheme that maintains the dependence of quadrats along a transect
perm <- how(within = Within(type = "free", mirror = TRUE), 
            plots = Plots(strata = transect$site, mirror = TRUE, type = "free"), 
            nperm = 999,
            blocks = NULL) 

survey_perma <- adonis2(survey_comm_data ~ salinity_regime * month, 
                        data = transect, 
                        permutations = perm, 
                        method = "bray") # take site out of adonis call 

survey_perma

df <- data.frame(treatments = c("salinity region", "month", "salinity region x month", "residuals", "total"), df = survey_perma$aov.tab$Df, SS = survey_perma$aov.tab$SumsOfSqs, MS = survey_perma$aov.tab$MeanSqs, `pseudo F` = survey_perma$aov.tab$F.Model, R2 = survey_perma$aov.tab$R2, `p value` = survey_perma$aov.tab$`Pr(>F)`)

df[ , 3:6] <- round(df[, 3:6], 2)


# Betadisp test -----------------------------------------------------------

# analogous to Levene's test for homogeneity of variances.

# standardize data with wisconsin method, then calculate a matrix of Bray-Curtis distances
dist <- vegdist(wisconsin(survey_comm_data), method = "bray")

levels(transect$salinity_regime) <- c("Low", "High")

# betadisper for salinity regions
dispersion_salinity <- betadisper(dist, group = transect$salinity_regime)
survey_disp_salnity <- permutest(dispersion_salinity) # equal variance between salinity regimes

survey_disp_salnity_df <- data.frame(Df = survey_disp_salnity$tab$Df, 
                                     SS = survey_disp_salnity$tab$`Sum Sq`,
                                     MS = survey_disp_salnity$tab$`Mean Sq`, 
                                     `F` = survey_disp_salnity$tab$`F`,
                                     N.Perm = survey_disp_salnity$tab$N.Perm,
                                     `Pr(>F)` = survey_disp_salnity$tab$`Pr(>F)`) %>% 
  mutate(across(where(is.numeric), round, 3)) %>% 
  rename(`Pr(>F)` = `Pr..F.`)

survey_disp_salnity_df[is.na(survey_disp_salnity_df)] <- ""

write_csv(survey_disp_salnity_df, here::here("results", "survey_disp_salnity.csv"))

survey_disp_salnity_df %>% 
  kableExtra::kable(align = "lccrr") %>% 
  kableExtra::kable_classic() %>% 
  kableExtra::kable_styling(full_width = F,
                            position = "left") %>% 
  kableExtra::save_kable("./figures/transect_salinity_betadisper.png")

# betadisper for months
levels(transect$month) <- c("May", "June", "July", "Aug")
dispersion_month <- betadisper(dist, group=transect$month)
survey_disp_month <- permutest(dispersion_month)# different variances between months

TukeyHSD(dispersion_month) # May and June have different amounts of variance compared to July

survey_disp_month_df <- data.frame(Df = survey_disp_month$tab$Df, 
                                   SS = survey_disp_month$tab$`Sum Sq`,
                                   MS = survey_disp_month$tab$`Mean Sq`, 
                                   `F` = survey_disp_month$tab$`F`,
                                   N.Perm = survey_disp_month$tab$N.Perm,
                                   `Pr(>F)` = survey_disp_month$tab$`Pr(>F)`) %>% 
  mutate(across(where(is.numeric), round, 3)) %>% 
  rename(`Pr(>F)` = `Pr..F.`)

survey_disp_month_df[is.na(survey_disp_month_df)] <- ""

write_csv(survey_disp_month_df, here::here("results", "survey_disp_month.csv"))

survey_disp_month_df %>% 
  kableExtra::kable(align = "lccrr") %>% 
  kableExtra::kable_classic() %>% 
  kableExtra::kable_styling(full_width = F,
                            position = "left") %>% 
  kableExtra::save_kable("./figures/transect_month_betadisper.png")

survey_disp <- rbind(survey_disp_salnity_df, survey_disp_month_df)

survey_disp %>% 
  kableExtra::kable(align = "lccrr") %>% 
  kableExtra::kable_classic() %>% 
  kableExtra::kable_styling(full_width = F,
                            position = "left") %>% 
  kableExtra::pack_rows("dispersion by salinity region", 1, 2) %>%
  kableExtra::pack_rows("dispersion by month", 3, 4) %>% 
  kableExtra::save_kable("./figures/transect_betadisper.png")

# plots

png("survey_betadisper_plot_A.png")
plot(dispersion_salinity, hull = FALSE, ellipse = TRUE, main = "", 
     col = c("orange", "steelblue")) # sd ellipse
title("A", adj = 0)
dev.off()

png("survey_betadisper_plot_B.png")
plot(dispersion_month, hull = FALSE, ellipse = TRUE, main="", 
col = wes_palette("Zissou1", 4, type = "continuous")) # sd ellipse
title("B", adj = 0)
dev.off()

png("survey_betadisper_month.png")
plot(dispersion_month, hull=FALSE, ellipse=TRUE, ) ## sd ellipse
dev.off()

png("survey_betadisper_salinity.png")p
plot(dispersion_salinity, hull=FALSE, ellipse=TRUE, col = c("orange", "steelblue")) ##sd ellipse
dev.off()



