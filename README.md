# Salinity controls rocky intertidal community structure via suppression of herbivory

This repository contains all the data and code used for the following manuscript: **Salinity controls rocky intertidal community structure via suppression of herbivory**, submitted to *Ecology* 

## Environmental data

- [salinity_discharge_figure.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/salinity_discharge_figure.R) uses the raw data files [Salinity Data.csv](https://github.com/sandraemry/SalinityandLimpets/blob/main/data/raw/Salinity%20Data.csv) and [Discharge.csv](https://github.com/sandraemry/SalinityandLimpets/blob/main/data/raw/Discharge.csv) to create [Fig. 2](https://github.com/sandraemry/SalinityandLimpets/blob/main/figures/salinity_discharge.jpeg)
- salinity data for each site is analysed in the following script: [salinity_t-test.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/salinity_t-test.R) which uses the raw data file [Salinity Data.csv](https://github.com/sandraemry/SalinityandLimpets/blob/main/data/raw/Salinity%20Data.csv). 
- [site_characteristic_table.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/site_characteristic_table.R) produces [Table S1](https://github.com/sandraemry/SalinityandLimpets/blob/main/figures/site_characteristics.png)


## Field surveys
- The raw data for the field surveys [Raw_Transect_data.csv](https://github.com/sandraemry/SalinityandLimpets/blob/main/data/raw/Raw_Transect_data.csv) was tidied with the following script: [field_transect_data_tidying.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/field_transect_data_tidying.R), which produces the dataset [field_transect_tidy.csv](https://github.com/sandraemry/SalinityandLimpets/blob/main/data/tidy/field_transect_tidy.csv). 

- [ordination_transects.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/ordination_transects.R) includes the code for [Fig. 3](https://github.com/sandraemry/SalinityandLimpets/blob/main/figures/survey_nmds.png), permanova analysis, and [Table S2](https://github.com/sandraemry/SalinityandLimpets/blob/main/figures/transect_betadisper.png) 

- [simper_transects.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/simper_transects.R) includes the code for the simper analysis and [Table S3](https://github.com/sandraemry/SalinityandLimpets/blob/main/figures/simper_table_fieldexp.png) 

- [transect_species_plot.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/transect_species_plots.R) creates [Fig. S1](https://github.com/sandraemry/SalinityandLimpets/blob/main/figures/transect_multispecies_plot_SM.png)


## Lab Experiments

### Limpet salinity tolerance
- Analysis for the salinity tolerance x population experiment is included in the [limpet_population_survival_analysis.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/limpet_population_survival_analysis.R) script, and uses the raw data file [limpet_mortality_local_adaptation.csv](https://github.com/sandraemry/SalinityandLimpets/blob/main/data/raw/limpet_mortality_local_adaptation.csv). This script also produces [Fig. 4](https://github.com/sandraemry/SalinityandLimpets/blob/main/figures/survival_plots_grid.png)

- Analysis for the salinity tolerance x tidal emersion experiment in the supplementary material is included in the [limpet_tidal_emersion_survival_analysis.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/limpet_tidal_emersion_survival_analysis.R) script, and uses the raw data files: [limpet_salinity_tolerance_tidal_pelta_exp.csv](https://github.com/sandraemry/SalinityandLimpets/blob/main/data/raw/limpet_salinity_tolerance_tidal_pelta_exp.csv) and [limpet_salinity_tolerance_tidal_digitalis_exp.csv](https://github.com/sandraemry/SalinityandLimpets/blob/main/data/raw/limpet_salinity_tolerance_tidal_digitalis_exp.csv). This script also produces [Fig. S2](https://github.com/sandraemry/SalinityandLimpets/blob/main/figures/transect_betadisper.png)  


### Ulva salinity tolerance
- Analysis for the ulva salinity tolerance experiment is included in the [ulva_salinity_tolerance.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/ulva_salinity_tolerance.R) script, and uses the tidy data files [ulva_mass_expmt.csv](https://github.com/sandraemry/SalinityandLimpets/blob/main/data/tidy/ulva_mass_expmt.csv) and [ulva_etrmax_expmt.csv](https://github.com/sandraemry/SalinityandLimpets/blob/main/data/tidy/ulva_etrmax_expmt.csv). This script also produces [Fig. 5](https://github.com/sandraemry/SalinityandLimpets/blob/main/figures/ulva_tolerance.png)


## Field grazer manipulation experiment 

- The raw data for the field experiment [Raw_Count_Data.csv](https://github.com/sandraemry/SalinityandLimpets/blob/main/data/raw/Raw_Count_Data.csv) were tidied with the following script: [field_exclusion_data_tidying.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/field_exclusion_data_tidying.R), which produces the dataset [field_exclusion_tidy.csv](https://github.com/sandraemry/SalinityandLimpets/blob/main/data/tidy/field_exclusion_tidy.csv). 

- [ordination_fieldexclusions.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/ordination_fieldexclusions.R) includes the code for [Fig. 6](https://github.com/sandraemry/SalinityandLimpets/blob/main/figures/exp_nmds.png), permanova analysis, and [Table S4](https://github.com/sandraemry/SalinityandLimpets/blob/main/figures/exp_betadisper.png)

- [simper_fieldexp.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/simper_fieldexp.R) includes the code for the simper analysis and [Table S5](https://github.com/sandraemry/SalinityandLimpets/blob/main/figures/simper_table_fieldexp.png)

- [field_exp_species_plot.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/field_exp_species_plot.R) creates [Fig. 7](https://github.com/sandraemry/SalinityandLimpets/blob/main/figures/means_errors_ulva_chthamalus.png) and [Fig. S3](https://github.com/sandraemry/SalinityandLimpets/blob/main/figures/multi_species_plot_SM.png)

- [ulva.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/ulva.R) includes the code for the model fitted to the ulva abundance data in the field experiment

- [chthamalus.R](https://github.com/sandraemry/SalinityandLimpets/blob/main/scripts/chthamalus.R) includes the code for the model fitted to the chthamalus abundance data in the field experiment


