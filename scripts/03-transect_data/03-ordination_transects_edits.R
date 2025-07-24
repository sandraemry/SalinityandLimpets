# # Ordination for the transect data that incorporates the reviewers suggestions 

library(tidyverse)
library(vegan)
# install.packages("remotes")
# remotes::install_github("jfq3/ggordiplots")
library(ggordiplots)

# Data prep ---------------------------------------------------------------

# read in data set for field transects
transect <- read_csv("./data/tidy/field_transect_tidy.csv")

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


# Betadisper --------------------------------------------------------------

one_way <- transect %>% 
  select(month,region) %>% 
  mutate(group = paste(region,month,sep=":"))

# permutation scheme that maintains the dependence of quadrats along a transect
perm <- how(within = Within(type = "free", mirror = TRUE), 
            plots = Plots(strata = transect$site, mirror = TRUE, type = "free"), 
            nperm = 999,
            blocks = NULL) 

# Use fourth root with range stdization
dist <- vegdist(decostand(survey_comm_data^(1/4),"range"), method = "bray")

dispersion_one.way <- betadisper(dist, group = one_way$group, type = "centroid")
survey_disp_one.way <- permutest(dispersion_one.way, permutations = perm) 
survey_disp_one.way


# Permanova ---------------------------------------------------------------
set.seed(3000)

## Use fourth-root transform and range standardization ****
survey_perma4 <- adonis2(dist ~ region*month, 
                         data = transect, 
                         permutations = perm,
                         by = "terms")
survey_perma4


# CAP ---------------------------------------------------------------------

survey_cap4 <- capscale(dist ~ region * month, 
                        data = transect)
survey_cap4

## Make groups based on region and month
categories <- transect %>% 
  select(month,region) %>% 
  mutate(group = paste(region,month,sep=":"))

# Extract the scores from the capscale object for plotting
cap_scores <- scores(survey_cap4, display = "sites")  # Extract site scores

# Convert the scores into a data frame
cap_df <- as.data.frame(cap_scores)
cap_df$group <- categories$group  # Add the grouping factor to the data
cap_df$Month <- categories$month
cap_df$Salinity <- categories$region

# Create an ordination plot with ggplot2
fig3 <- ggplot(cap_df, aes(x = CAP1, y = CAP2)) +
  geom_point(aes(colour = Salinity), size = 3, alpha = 0.8) + 
  scale_colour_manual(values = c("orange", "steelblue"))  + 
  stat_ellipse(aes(color = Salinity, linetype = Month),
               type = "t",
               level = 0.95) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotdash", "dotted")) + 
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

ggsave(plot = fig3, file = "./figures/fig3_new.jpg")
