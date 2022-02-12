## Field exclusion data - Jan 14, 2020 
## Data Tidying

library(tidyverse)

field_exp <- read_csv(here::here("data", "raw", "Raw_Count_Data.csv"))

#filter out just July 
field_exp <- field_exp %>% 
  filter(Month == "July") %>% # taking out the other months, as nothing had really recruited until then
  filter(Treatment != "I") #taking out the inclusion treatments since they weren't effective in the low salinity sites 
  
  # renaming some variables
field_exp <- field_exp %>% 
  dplyr::rename(balanus_no = `Balanus (#)`, 
         chthamalus_no = `Chthamalus (#)`,
         unknown_limpets_no = `Unknown Limpets`, 
         sitkana_no = `Littorina sitkana (#)`, 
         littorina_spp_no = `Littorina spp. (#)`, 
         mytilus_pt = `Mytilus (%)`,
         anemone_pt = `Anemone (%)`,
         masto_crust_pt = `Masto Crust (%)`,
         mastocarpus_pt = `Mastocarpus (%)`,
         hildenbrandia_pt = `Hildenbrandia (%)`,
         diatom_pt = `Diatom Cover (%)`,
         enteromorpha_pt = `Enteromorpha (%)`,
         ulva_pt = `Ulva (%)`,
         urospora_pt = `Urospora (%)`, 
         pyropia_pt = `Porphyra (%)`,
         fucus_pt = `Fucus (%)`,
         salinity = `Salinity (ppt)`, 
         greens_pyropia = `Greens+Porphyra`, 
         reds_fucus = `Reds+Fucus`,
         digitalis_no = Digitalis, 
         pelta_no = Pelta, 
         persona_no = Persona, 
         paradigitalis_no = Paradigitalis, 
         scutum_no = Scutum,
         amphipod_no = Amphipod)

field_exp <- field_exp %>% 
  mutate(mytilus_pt = mytilus_pt * 100, 
         anemone_pt = anemone_pt * 100, 
         masto_crust_pt = masto_crust_pt * 100, 
         mastocarpus_pt = mastocarpus_pt * 100, 
         hildenbrandia_pt = hildenbrandia_pt * 100, 
         diatom_pt = diatom_pt * 100, 
         enteromorpha_pt = enteromorpha_pt * 100, 
         ulva_pt = ulva_pt* 100, 
         urospora_pt = urospora_pt * 100, 
         pyropia_pt = pyropia_pt * 100, 
         fucus_pt = fucus_pt * 100) %>% 
  mutate(ulva_pt = ulva_pt + enteromorpha_pt)
  
    
names(field_exp) <- tolower(names(field_exp))

field_exp <- field_exp %>% 
  fill(date, time, salinity, .direction = "down") %>% 
  select(date, time, salinity, month, region, site, replicate, treatment, 
         balanus_no, chthamalus_no, digitalis_no, pelta_no, persona_no, paradigitalis_no, 
         scutum_no, unknown_limpets_no, sitkana_no, littorina_spp_no, mytilus_pt, 
         anemone_pt, amphipod_no, masto_crust_pt, mastocarpus_pt, hildenbrandia_pt, 
         diatom_pt, ulva_pt, urospora_pt, pyropia_pt, fucus_pt, 
         greens, reds, greens_pyropia, reds_fucus)
  

# get rid of any species that has 0s in all quadrats, 
# do we want to do this? They were measured but absent in all?
field_exp_metadata <- field_exp[ ,1:8]
field_exp_data <- field_exp[ ,9:33]
field_exp_data <- field_exp_data[ , (colMeans(field_exp_data[ , ]) != 0)]
field_exp <- cbind(field_exp_metadata, field_exp_data)

rm(field_exp_metadata, field_exp_data)

write_csv(field_exp, "./data/tidy/field_exclusion_tidy.csv")

