## simper analysis on transect data
dist <- vegdist(wisconsin(survey_comm_data), method = "bray")

transect_salinity_species_contributions <- simper(wisconsin(survey_comm_data), group = survey_env_data$salinity_regime, permutations = perm)

transect_month_species_contributions <- simper(wisconsin(survey_comm_data), group = survey_env_data$month, permutations = perm)

# a <- summary(transect_salinity_species_contributions)
salinity_species_contribution <- tibble(species = transect_salinity_species_contributions$H_L$species,
       contribution = transect_salinity_species_contributions$H_L$average)


variance <- sum((survey_comm_data[ ,1] - mean(as_vector(survey_comm_data[ ,1])))^2)/(nrow(survey_comm_data) - 1)
for (i in 2:ncol(survey_comm_data)){
  a <- sum((survey_comm_data[ ,i] - mean(as_vector(survey_comm_data[ ,i])))^2)/(nrow(survey_comm_data) - 1)
  variance <- append(variance, a)
}


variance <- tibble(variance, species = names(survey_comm_data), )
var_cont <- left_join(variance, salinity_species_contribution, by = "species") %>% 
  arrange(variance)

var_cont %>% 
  filter(variance != max(variance)) %>% 
  ggplot(.) + 
  geom_point(aes(x = log(variance), y = contribution)) + 
  
