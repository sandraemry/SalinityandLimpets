# Trying out the ecotraj package on the Coyle data
# January 5, 2022

library(ecotraj)
library(here)
library(vegan)
library(dplyr)
library(readr)

data <- read_csv(here::here("data", "tidy", "field_transect_tidy.csv"))

data <- data %>%
  mutate(month = factor(month, levels = c("May", "June", "July", "September")),
         site = factor(site, levels = c("Eagle Cove", "Hailstorm 2", "Ruckle", "Horseshoe Bay", "Lions Bay 2", "Rope Site 2"))) %>% 
  arrange(site, month)


species_data <- data %>% 
  dplyr::group_by(site, month) %>% 
  dplyr::summarize(balanus_pt = mean(balanus_pt),
            chthamalus_pt = mean(chthamalus_pt),
            barnacle_recruits_pt = mean(barnacle_recruits_pt), 
            mytilus_pt = mean(mytilus_pt),
            fucus_pt = mean(fucus_pt), 
            ulva_pt = mean(ulva_pt),
            hildenbrandia_pt = mean(hildenbrandia_pt),
            mastocarpus_pt = mean(mastocarpus_pt),
            mastocarpus_crust_pt = mean(mastocarpus_crust_pt),
            pyropia_pt = mean(pyropia_pt),
            endocladia_pt = mean(endocladia_pt),
            microcladia_pt = mean(microcladia_pt),
            polysiphonia_pt = mean(polysiphonia_pt),
            diatoms_pt = mean(diatoms_pt),
            urospora_pt = mean(urospora_pt),
            acrosiphonia_pt = mean(acrosiphonia_pt),
            melanosiphon_pt = mean(melanosiphon_pt),
            leathesia_pt = mean(leathesia_pt),
            littorina_spp_no = mean(littorina_spp_no),
            sitkana_no = mean(sitkana_no),
            paradigitalis_no = mean(paradigitalis_no),
            unknown_limpet_no = mean(unknown_limpet_no),
            pelta_no = mean(pelta_no),
            scutum_no = mean(scutum_no),
            persona_no = mean(persona_no),
            digitalis_no = mean(digitalis_no),
            anemone_pt = mean(anemone_pt),
            hemigrapsis_no = mean(hemigrapsis_no),
            hermit_crab_no = mean(hermit_crab_no)) %>% 
  dplyr::ungroup()

sites <- species_data$site
surveys <- species_data$month
  
comm_data <- species_data %>% 
  select(-c( , 1:2))

# matrix of bray curtis distances on wisconsin transformed data:
dist <- vegdist(wisconsin(comm_data), method = "bray")

png("survey_traj.png")
trajectoryPCoA(dist, sites, surveys, 
               traj.colors = c("steelblue", "slateblue", "mediumblue", "orange", "sienna1", "tomato4"), lwd = 2, survey.labels = T)
legend("bottomright", col = c("steelblue", "slateblue", "mediumblue", "orange", "sienna1", "tomato4"), 
       legend = c("Ruckle", "Eagle Cove", "Hailstorm", "Horseshoe Bay", "Lions Bay", "Rope Site"), bty = "n", lty = 1, lwd = 2)
dev.off()