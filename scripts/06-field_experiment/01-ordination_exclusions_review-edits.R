# Ordination for the field experiment that incorporates the reviewers suggestions 

library(tidyverse)
library(vegan)
# install.packages("remotes")
# remotes::install_github("jfq3/ggordiplots")
library(ggordiplots)



# Data prep ---------------------------------------------------------------

# Load in data
exp <- read_csv("./data/tidy/field_exclusion_tidy.csv")

# create unique row name indicators
exp <- exp %>% 
  unite(col = "site_rep_treatment", c("site", "replicate", "treatment"), sep = "_", remove = FALSE) %>% 
  mutate(treatment = recode_factor(treatment, C = "Control", E = "Exclusion"), 
         site = factor(site, levels = c("EC", "HS", "RP", "HB", "LB", "RS")),
         region = factor(region, levels = c("Low", "High")))

# getting rid of redundant variables 
# This is just the species data 
exp_comm_data <- exp %>% 
  dplyr::select(balanus_no:fucus_pt)

# This is just the site data
exp_env_data <- exp %>%
  dplyr::select(site, region, treatment, replicate) %>% 
  dplyr::mutate(region = factor(region))



# Dispersion test ---------------------------------------------------------

## PSP: Do Betadisper test first: none of the transformations or standardizations work
## PSP: Do Betadisper using groups in a one-way design

categories <- exp %>% 
  select(treatment,region) %>% 
  mutate(group = paste(region,treatment,sep=":"))

exp_env_data <- exp %>%
  dplyr::select(site, region, treatment, replicate) %>% 
  dplyr::mutate(region = factor(region))

dist4 <- vegdist(decostand(exp_comm_data^(1/4),"range"), method = "bray")

d.4 <- betadisper(dist4, group = categories$group)
permutest(d.4,permutations = perm_replicate) 
permutest(d.4,permutations = perm_site) # doesn't matter what the scheme is, same results
permutest(d.4)

# d.wis <- betadisper(dist.wis, group = categories$group)
# permutest(d.wis,permutations = perm_replicate) 
# permutest(d.wis,permutations = perm_site)
# permutest(d.wis)

# BETADISP test for salinity region
dispersion_salinity <- betadisper(dist.wis, group = exp_env_data$region)
exp_disp_salinity <- permutest(dispersion_salinity,permutations = perm_replicate) 
exp_disp_salinity

# BETADISP test for treatment groups 
dispersion_treatment <- betadisper(dist.wis, group=exp_env_data$treatment)
exp_disp_treatment <- permutest(dispersion_treatment,permutations = perm_replicate) # greater dispersion in the control
exp_disp_treatment
# Permanova ---------------------------------------------------------------

#setting up the permutation scheme - using PSP's recommendation 
exp_env_data_paired <- exp %>%
  select(site, region, treatment, site_rep_treatment, replicate) %>% 
  rename(unique_rep = site_rep_treatment) %>% 
  mutate(group = paste(region,treatment,sep="."), group = factor(group))


# perm_site <- how(within = Within(type = "free"), 
#                       plots = Plots(strata = exp_env_data_paired$site, type = "free"), 
#                       blocks = NULL,
#                       nperm = 999) 

perm_replicate <- how(within = Within(type = "free"), 
                      plots = Plots(strata = exp_env_data_paired$unique_rep, type = "free"), 
                      blocks = NULL,
                      nperm = 999) 

set.seed(2000)
## fourth-root transform and standardize by range
exp_perma_site4 <- adonis2(decostand(exp_comm_data^(1/4),"range") ~ region * treatment, 
                           data = exp_env_data, 
                           permutations = perm_replicate, 
                           method = "bray",
                           by = "terms")
exp_perma_site4


# CAP plot ---------------------------------------------------------------

comm_dist <- vegdist(decostand(exp_comm_data^(1/4),"range"), method = "bray")

cap_analysis4 <- capscale(comm_dist ~ region * treatment, 
                          data = exp_env_data)

## Make groups based on region and treatment
categories <- exp_env_data %>% 
  select(treatment,region) %>% 
  mutate(group = paste(region,treatment,sep=":"))

# # plot clearly shows the expectation is (HE = LE = LC) not equal to HC
# # When kind = "se": The conf = 0.95 indicates the standard error ellipses 
# # will cover the region within 95% of the standard error around the centroid 
# # of each group.
# gg_ordiplot(cap_analysis4, 
#             groups = categories$group, 
#             kind = "sd", 
#             conf = 0.95, 
#             pt.size = 1)


# Extract the scores from the capscale object for plotting
cap_scores <- scores(cap_analysis4, display = "sites")  # Extract site scores

# Convert the scores into a data frame
cap_df <- as.data.frame(cap_scores)
cap_df$group <- categories$group  # Add the grouping factor to the data
cap_df$Treatment <- categories$treatment
cap_df$salinity <- categories$region

# Create an ordination plot with ggplot2
fig6 <- ggplot(cap_df, aes(x = CAP1, y = CAP2)) +
  geom_point(aes(shape = Treatment, colour = salinity), size = 3, alpha = 0.8) + 
  scale_colour_manual(values = c("orange", "steelblue"))  + 
  stat_ellipse(aes(color = salinity, linetype = Treatment),
               type = "t",
               level = 0.95) + 
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

  
ggsave(plot = fig6, file = "./figures/figure6_new.jpg")
