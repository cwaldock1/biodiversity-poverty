#### Script to process variables as predictors of colonisation and poverty
# devtools::install_github("jslefche/piecewiseSEM@devel")
pacman::p_load(tidyverse, dplyr, tidyr, sf, tmap, terra, ggh4x, mclust, GGally, rworldxtra, broom, tidyr, 
               foreign, wbstats, janitor, htmlwidgets, webshot, piecewiseSEM, semEff, lavaan, psych, tidySEM)




#### Ertan et al. 2015  ----

ertan_data <- read.dta("data/Ertan_2015_who_was_colonized_and_when/data/Ertan_Fiszbein_Putterman_EER.dta")

ertan_data2 <- ertan_data %>% 
  select(country, wbcode, col, agyears_including_colonizers, statehist_including_colonizers, 
         tech1500_including_colonizers, navdist, landdist, EDE, landlocked, latitude) %>% 
  mutate(across(col:landlocked, ~ replace(., is.na(.), 0)),
         wbcode = case_when(wbcode == 'ZAR' ~ 'CD', 
                            wbcode == 'OAN' ~ 'TW', 
                            T ~ wbcode), 
         latitude = abs(latitude)) %>% 
  mutate(latitude = case_when(country == 'Belgium' ~ 51,
                              country == 'Germany' ~ 51,
                              country == 'Spain' ~ 38,
                              country == 'France' ~ 45,
                              country == 'United Kingdom' ~ 55,
                              country == 'England' ~ 52,
                              country == 'Italy' ~ 43,
                              country == 'Netherlands' ~ 52,
                              country == 'Portugal' ~ 40, 
                              T ~ latitude))

#### Acemoglu et al. 2001 ----

# available in image so can type up manually -> however only 60 countries

#### World bank data portal ----

wb_search('poverty headcount')
#CC.PER.RNK = control of corruption percentile range
#RL.PER.RNK = rule of law percentile rank
#GE.PER.RNK = government effectiveness
#PV.PER.RNK = political stability and absences of violence
#RQ.PER.RNK = regulatory quality
#VA.PER.RNK = voice and accountability
#DT.TDS.DECT.EX.ZS = total debt as % of GDP
#AG.LND.AGRI.ZS = agricultural land %


my_indicators = c("corruption" = "CC.PER.RNK",
                  "law" = "RL.PER.RNK",
                  "governance" = "GE.PER.RNK",
                  "political_stability" = "PV.PER.RNK",
                  "regulations" = "RQ.PER.RNK",
                  "voice" = "VA.PER.RNK",
                  "debt" = "DT.TDS.DECT.EX.ZS",
                  "agriculture" = "AG.LND.AGRI.ZS", 
                  "poverty_headcount" = 'SI.POV.DDAY', 
                  'national_poverty_headcount' = 'SI.POV.NAHC', 
                  'MD_poverty' = 'SI.POV.MPUN', 
                  'gini' = 'SI.POV.GINI', 
                  'resource_rent' = 'NY.GDP.TOTL.RT.ZS')

wb_download <- wb_data(my_indicators, country = ertan_data2$wbcode, start_date = 2000, end_date = 2024)

wb_clean <- wb_download %>% 
  group_by(iso2c, iso3c, country) %>% 
  summarise(across(agriculture:voice, mean, na.rm = T)) %>% 
  # assume NA debt is none
  #mutate(across(debt, ~ replace(., is.na(.), 0))) %>% 
  ungroup()

write_csv(wb_clean, 'data/wb_clean_20250410.csv')

#### read in poverty and biodiversity data----

cpb <- read_sf('data/country_poverty_biodiversity/country_poverty_biodiversity.shp')
cpb_names <- names(read_csv('data/country_poverty_biodiversity/country_poverty_biodiversity_names_3.csv'))
names(cpb)[1:length(cpb_names)] <- cpb_names

# subset and clean
cpb_processed <- cpb %>% 
  select(`alpha-3`, COUNTRY, IUCN_combined_SR, plant_alpha_SR, mammal_gd, pa_coverage) %>% 
  st_drop_geometry() %>% 
  clean_names

#### read in mineral resources data ----

minerals <- read_csv('data/mrds-trim/clean_minerals.csv')


#### combine data sources ----

social_data <- left_join(wb_clean, ertan_data2 %>% select(-country), by = c('iso3c' = 'wbcode')) %>% 
  filter(!is.na(col))
  
all_data <- left_join(social_data, cpb_processed %>% select(-country), by = c('iso3c' = 'alpha_3'))

all_data <- left_join(all_data, minerals %>% 
            filter(!country %in% c('Canarias', 'Azores', 'Madeira')) %>% # remove one to many matches to keep only mainland country
            select(-country),
          by = c('iso2c' = 'iso')) 

# standardize data
all_data <- all_data %>% 
  #mutate(across(agriculture:pa_coverage, scales::rescale)) %>% 
  # clean settler colonies and contentious and shortly influenced to be non-colonised
  mutate(col = case_when(country %in% c('Australia', 'New Zealand', 
                                        'United States', 'South Africa', 
                                        'Canada', 'Israel', 'Singapore', 
                                        'Argentina', 'Uruguay', 
                                        'Syria', 'Jordan', 'Iraq', 
                                        'Ethiopia', 
                                        'Taiwan') ~ 0, 
                         T ~ col)) %>% 
  # make debt binary
  mutate(debt = case_when(is.na(debt) ~ 0, T ~ 1), 
         log_resource_rent = log(resource_rent)) %>% 
  rename(mineral_locations = point_count, 
         mineral_locations_log10 = point_count_log10)

#### Build data table that will be used as input and explore correltions ----

head(all_data)

names(all_data)

pacman::p_load()

# check distribution of biodiversity metrics and add column for sqrt of iucn
all_data$iucn_combined_sr %>% na.omit %>% sqrt %>%  hist
all_data$iucn_combined_sr_sqrt <- sqrt(all_data$iucn_combined_sr)

# check correlations for predictors of colonisation
pairs.panels(all_data %>%  select(statehist_including_colonizers, 
                                  agyears_including_colonizers, 
                                  tech1500_including_colonizers, 
                                  navdist, 
                                  landdist, 
                                  latitude,
                                  iucn_combined_sr_sqrt, 
                                  log_resource_rent, 
                                  mineral_locations_log10))
# OK cannot use state, agriculture years and technology together
# should consider latitude as a drive of richness

# check correlations for predictors of poverty
pairs.panels(all_data %>%  select(debt, landlocked, agriculture, iucn_combined_sr_sqrt))
# no variables are strongly correlated

# aditional variables not used
pairs.panels(all_data %>%  select(law , governance , corruption , voice, log_resource_rent))   

# rename for easier interpretations of plots
all_data_clean <- all_data %>% 
  rename(colonisation = col,
         prehistory_states = statehist_including_colonizers, 
         maritime_distance = navdist, 
         land_distance = landdist, 
         range_richness = iucn_combined_sr, 
         plant_richness = plant_alpha_sr, 
         poverty_rate = poverty_headcount, 
         governance_strength = governance, 
         minerals = mineral_locations_log10) %>% 
  
  # convert values to scale between 0 and 1 from percentages
  mutate(poverty_rate = poverty_rate / 100, 
         national_poverty_headcount = national_poverty_headcount/100, 
         MD_poverty = MD_poverty/100, 
         gini = gini/100, 
         governance_strength = governance_strength/100) %>% 
  
  # select only those variables of interest
  select(country, colonisation, prehistory_states, maritime_distance, land_distance, 
         poverty_rate, debt, governance_strength, 
         landlocked, agriculture, latitude, 
         log_resource_rent, minerals,
         range_richness, plant_richness, mammal_gd, 
         poverty_rate, national_poverty_headcount, MD_poverty, gini) %>% 
  
  unique()


# check data distributions
all_data_clean %>% 
  unique() %>% 
  summarytools::dfSummary(.) %>% summarytools::view()

saveRDS(all_data_clean, 'data/SEM_dataInput.RDS')




#### Create output directory ----

fig_dir <- 'figures/SEM_analysis/model_summaries'
dir.create(fig_dir, recursive = T)

#### Create set of baseline SEM for range richness ----

rr_data <- all_data_clean %>% 
  # select all variables
  select(range_richness, latitude, log_resource_rent, prehistory_states, colonisation,
         maritime_distance, land_distance, log_resource_rent, governance_strength, 
         debt, poverty_rate, landlocked, agriculture) %>% 
  na.omit %>% 
  data.frame

# specify peicewiseSEM
rr_sem <- psem(
  
  # richness predicted by latitude
  lm(range_richness ~ latitude, data = rr_data),
  lm(log_resource_rent ~ latitude, data = rr_data),
  lm(prehistory_states ~ range_richness, data = rr_data),
  
  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ prehistory_states + maritime_distance + land_distance + log_resource_rent + range_richness, family = 'binomial', data = rr_data),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = rr_data),
  
  # debt predicted by colonisation history
  glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = rr_data),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(poverty_rate ~ debt + governance_strength + landlocked + agriculture + log_resource_rent + range_richness, 
      family = 'binomial', data = rr_data),
  
  data = rr_data 
  
  )

fisherC(rr_sem) # model is not valid # p<0.05
summary(rr_sem)
plot(rr_sem, layout = 'tree')


# perform model selection 
rr_sem_sel <- backward_selection(rr_sem,  save_path = file.path(fig_dir, 'rr_pr_full.csv'))
plot(rr_sem_sel, layout = 'tree')

### sequentially removing all non-significant links with lowest effect size
range_richness_subset_sem <- psem(
  
  # richness predicted by latitude
  lm(range_richness ~ latitude, data = all_data_clean),
  lm(log_resource_rent ~ latitude, data = all_data_clean),
  lm(prehistory_states ~ latitude, data = all_data_clean),
  
  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ range_richness + latitude, family = 'binomial', data = all_data_clean),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = all_data_clean),
  
  # debt predicted by colonisation history
  glm(debt ~ governance_strength, family = 'binomial', data = all_data_clean),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(poverty_rate ~  governance_strength, 
      family = 'binomial', data = all_data_clean),
  
  data = all_data_clean
  
)
fisherC(range_richness_subset_sem) # model is valid
summary(range_richness_subset_sem)
plot(range_richness_subset_sem, layout = 'tree')
range_richness_subset_sem_boot <- bootEff(range_richness_subset_sem, R = 100, seed = 13, parallel = "no")
range_richness_subset_sem_boot_withLat_effect <- semEff(range_richness_subset_sem_boot, predictor = "range_richness")
summary(range_richness_subset_sem_boot_withLat_effect)



### checking for model validity with no latitude
# specify peicewiseSEM
range_richness_noLat_psem <- psem(
  
  # natural resources initiate states
  lm(prehistory_states ~ range_richness, data = all_data_clean),
  
  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ prehistory_states + range_richness, family = 'binomial', data = all_data_clean),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = all_data_clean),
  
  # debt predicted by colonisation history
  glm(debt ~ governance_strength, family = 'binomial', data = all_data_clean),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(poverty_rate ~ governance_strength, 
      family = 'binomial', data = all_data_clean),
  
  data = all_data_clean
  
)
fisherC(range_richness_noLat_psem)
summary(range_richness_noLat_psem)
plot(range_richness_noLat_psem, layout = 'tree')
range_richness_noLat_psem_boot <- bootEff(no_lat_psem, R = 100, seed = 13, parallel = "no")
range_richness_noLat_psem_boot_effect <- semEff(range_richness_noLat_psem_boot, predictor = "range_richness")
summary(range_richness_noLat_psem_boot_effect)




#### Mammal GD ----

# specify peicewiseSEM
mammal_gd_sem <- psem(
  
  # richness predicted by latitude
  lm(mammal_gd ~ latitude, data = all_data_clean),
  lm(log_resource_rent ~ latitude, data = all_data_clean),
  lm(prehistory_states ~ mammal_gd, data = all_data_clean),
  
  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ prehistory_states + maritime_distance + land_distance + log_resource_rent + mammal_gd, family = 'binomial', data = all_data_clean),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = all_data_clean),
  
  # debt predicted by colonisation history
  glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = all_data_clean),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(poverty_rate ~ debt + governance_strength + landlocked + agriculture + log_resource_rent + mammal_gd, 
      family = 'binomial', data = all_data_clean),
  
  data = all_data_clean
  
)

fisherC(mammal_gd_sem) # model is not valid # p<0.05
summary(mammal_gd_sem)
plot(mammal_gd_sem, layout = 'tree')


### sequentially removing all non-significant links with lowest effect size
mammal_gd_subset_sem <- psem(
  
  # richness predicted by latitude
  lm(mammal_gd ~ latitude, data = all_data_clean),
  lm(log_resource_rent ~ latitude, data = all_data_clean),
  lm(prehistory_states ~ mammal_gd, data = all_data_clean),
  
  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ prehistory_states + mammal_gd, family = 'binomial', data = all_data_clean),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = all_data_clean),
  
  # debt predicted by colonisation history
  glm(debt ~ governance_strength, family = 'binomial', data = all_data_clean),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(poverty_rate ~ governance_strength, 
      family = 'binomial', data = all_data_clean),

  data = all_data_clean
  
)
fisherC(mammal_gd_subset_sem) # model is not valid based on fisherC
summary(mammal_gd_subset_sem)
plot(mammal_gd_subset_sem, layout = 'tree')
mammal_gd_subset_sem_boot <- bootEff(mammal_gd_subset_sem, R = 100, seed = 13, parallel = "no")
mammal_gd_subset_sem_boot_withLat_effect <- semEff(mammal_gd_subset_sem_boot, predictor = "mammal_gd")
summary(mammal_gd_subset_sem_boot_withLat_effect)



### checking for model validity with no latitude
# specify peicewiseSEM
mammal_gd_noLat_psem <- psem(
  
  # richness predicted by latitude
  lm(prehistory_states ~ mammal_gd, data = all_data_clean),
  
  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ prehistory_states  + mammal_gd, family = 'binomial', data = all_data_clean),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = all_data_clean),
  
  # debt predicted by colonisation history
  glm(debt ~ governance_strength, family = 'binomial', data = all_data_clean),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(poverty_rate ~ governance_strength, 
      family = 'binomial', data = all_data_clean),
  
  data = all_data_clean
  
)
fisherC(mammal_gd_noLat_psem)
summary(mammal_gd_noLat_psem)
plot(mammal_gd_noLat_psem, layout = 'tree')
mammal_gd_noLat_psem_boot <- bootEff(mammal_gd_noLat_psem, R = 100, seed = 13, parallel = "no")
mammal_gd_noLat_psem_boot_effect <- semEff(mammal_gd_noLat_psem_boot, predictor = "mammal_gd")
summary(mammal_gd_noLat_psem_boot_effect)


#### Testing latitude only -----

# specify peicewiseSEM
onlyLatitude_psem <- psem(
  
  # richness predicted by latitude
  lm(log_resource_rent ~ latitude, data = all_data_clean),

  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ latitude, family = 'binomial', data = all_data_clean),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = all_data_clean),
  
  # debt predicted by colonisation history
  glm(debt ~ governance_strength, family = 'binomial', data = all_data_clean),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(poverty_rate ~ governance_strength, 
      family = 'binomial', data = all_data_clean),
  
  data = all_data_clean
  
)

fisherC(onlyLatitude_psem)
summary(onlyLatitude_psem)
plot(onlyLatitude_psem, layout = 'tree')
onlyLatitude_psem_boot <- bootEff(onlyLatitude_psem, R = 100, seed = 13, parallel = "no")
mammal_gd_noLat_psem_boot_effect <- semEff(mammal_gd_noLat_psem_boot, predictor = "mammal_gd")
summary(mammal_gd_noLat_psem_boot_effect)



