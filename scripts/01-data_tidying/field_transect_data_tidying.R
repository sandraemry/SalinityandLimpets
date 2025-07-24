## Field Transect Data Tidying
# This script will take the csv Raw_Transect_data and change the names of the columns, 
# get rid of the log(species) columns, and filter to only July's data, and then write 
# a tidy csv called field_transect_tidy.csv
# January 18, 2021

library(tidyverse)

transect <- read_csv("./data/raw/Raw_Transect_data.csv")

transect <- transect %>% 
  dplyr::rename(salinity = `Salinity (ppt)`,
         region = `Salinity (H/L)`,
         quadrat_no = `Quadrat #`,
         balanus_pt = `Balanus (%)`,
         chthamalus_pt = `Chthamalus (%)`,
         barnacle_recruits_pt = `Barnacle Recruits (%)`,
         barnacles_total_pt = `All Barnacles`,
         mytilus_pt = `Mytilus (%)`,
         fucus_pt = `Fucus (%)`,
         ulva_pt = `Ulva (%)`,
         enteromorphora_or_ulva_pt = `Enteromorpha or Ulva intestinalis (%)`,
         hildenbrandia_pt = `Hildenbrandia (%)`,
         mastocarpus_pt = `Mastocarpus (%)`, 
         mastocarpus_crust_pt = `Mastocarpus Crust (%) Crust (%)`,
         pyropia_pt = `Porphyra (%)`,
         endocladia_pt = `Endocladia (%)`,
         microcladia_pt = `Microcladia (%)`,
         polysiphonia_pt = `Polysiphonia (%)`,
         diatoms_pt = `Diatoms (%)`,
         urospora_pt = `Urospora (%)`,
         acrosiphonia_pt = `Acrosiphonia (%)`,
         melanosiphon_pt = `Melanosiphon intestinalis (%)`,
         leathesia_pt = `Leathesia difformis (%)`,
         greens_pt = `Greens(Acro,uros,ulva,entero)`,
         greens_pyropia_pt = `Greens plus porphyra`,
         reds_pt = `Reds(melano,poly,micro,endo,masto,hilden)`,
         reds_fucus_pt = `Reds plus Fucus`, 
         littorina_spp_no = `Litorina spp. (#) (other) (#)`,
         sitkana_no = `Litorina sitkana (#) (sitkana) (#)`,
         littorina_total_no = `Litorina (total) (#)(total) (#)`,
         paradigitalis_no = `Paradigitalis (#)`,
         unknown_limpet_no = `Unknown Limpet (#)(#)`,
         pelta_no = `Pelta (#)`,
         scutum_no = `Scutum (#)`,
         persona_no = `Persona (#)`,
         digitalis_no = `Digitalis (#)`,
         all_limpets_no = `All Limpets`,
         gastropods_no = Gastropods,
         anemone_pt = `Anemone (%)`,
         oyster_pt = `Oyster (%)`,
         hemigrapsis_no = `Hemigrapsis (#)`,
         hermit_crab_no = `Hermit Crab (#)`) %>% 
  mutate(ulva_spp_pt = ulva_pt + enteromorphora_or_ulva_pt)

names(transect) <- tolower(names(transect))  

transect <- transect %>% 
  select(-c(`...49`, `log litt`, `log all limpets`, `log gastropods`, 
            other, enteromorphora_or_ulva_pt, greens_pt, greens_pyropia_pt, reds_pt, reds_fucus_pt,
            ulva_pt, barnacles_total_pt, all_limpets_no, littorina_total_no, gastropods_no))

# get rid of any species that has 0s in all quadrats, 
# do we want to do this? They were measured but absent in all?
transect_metadata <- transect[ ,1:5]
transect_data <- transect[ ,6:35]
transect_data <- transect_data[ , (colMeans(transect_data[ , ]) != 0)]
transect <- cbind(transect_metadata, transect_data)

rm(transect_data, transect_metadata)

#change site names to codes to match other files
transect$site <- str_replace(transect$site, "Ruckle", "RP") 
transect$site <- str_replace(transect$site, "Eagle Cove", "EC")
transect$site <- str_replace(transect$site, "Hailstorm 2", "HS")
transect$site <- str_replace(transect$site, "Horseshoe Bay", "HB")
transect$site <- str_replace(transect$site, "Lions Bay 2", "LB")
transect$site <- str_replace(transect$site, "Rope Site 2", "RS")

#change L and H to low and high, to match other data files
transect$region <- str_replace(transect$region, "L", "Low") 
transect$region <- str_replace(transect$region, "H", "High") 

write_csv(transect, "./data/tidy/field_transect_tidy.csv")
