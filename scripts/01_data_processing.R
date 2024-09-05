# Load in packages
pacman::p_load(tidyverse, sf, tmap, terra, pipr)
pacman::p_load_gh('worldbank/pipr')

# read in arcGIS provided data layers for world map with 2 digit ISO codes
esri_countries <- st_read('data/World_Countries_Generalized/World_Countries_Generalized.shp')
 
# view world map as sanity check
tm_shape(esri_countries) + 
  tm_borders()

#### The aim of the data processing below is: 
# - read in data
# - re-project to esri_countries object if necessary
# - extract mean values per country
# - save in a harmonized dataset a per country value for each metric

###### BIODIVERSITY DATASETS ----

#### a) IUCN species richness combined
sr <- rast('data/Combined_SR_2022/Combined_SR_2022.tif')
sr <- project(sr, crs(esri_countries))
sr_mean <- extract(sr, vect(esri_countries), fun = mean, na.rm = T, touches = T)
esri_countries$IUCN_combined_SR <-  sr_mean$Combined_SR_2022
tm_shape(esri_countries) + 
  tm_fill(col = 'IUCN_combined_SR', 
          style = 'cont')

#### b) Sabatini et al. 2022 Plant alpha diversity
plant_alpha <- rast('data/Sabatini et al. 2022 Plant alpha diversity/3506_70_w3_tile_joint_sr1000.tif')
plant_alpha <- project(plant_alpha, crs(esri_countries))
plant_alpha_mean <- extract(plant_alpha, vect(esri_countries), fun = mean, na.rm = T, touches = T)
esri_countries$plant_alpha_SR <-  plant_alpha_mean$X3506_70_w3_tile_joint_sr1000
tm_shape(esri_countries) + 
  tm_fill(col = 'plant_alpha_SR', 
          style = 'cont')

#### c) Theodoridis et al. 2020 Genetic diversity mammals
gd_co1 <- st_read("data/Theodoridis et al 2020 Genetic diversity mammals/predicted_GD/co1_predictedGD.json")
gd_co1 <- rasterize(vect(gd_co1), plant_alpha, field = 'GD')

gd_cytb <- st_read("data/Theodoridis et al 2020 Genetic diversity mammals/predicted_GD/cytb_predictedGD.json")
gd_cytb <- rasterize(vect(gd_cytb), plant_alpha, field = 'GD')

gd_rast <- app(c(gd_co1, gd_cytb), mean)

plot(gd_rast)

gd_rast_mean <- extract(gd_rast, vect(esri_countries), fun = mean, na.rm = T, touches = T)

esri_countries$mammal_gd <-  gd_rast_mean$mean
tm_shape(esri_countries) + 
  tm_fill(col = 'mammal_gd', 
          style = 'cont')

###### BIODIVERSITY MODIFICATION DATASETS ----

#### a) Sanchez-Ortiz et al. 2019 BII glo
bii_abund <- rast('data/Sanchez-Ortiz et al. 2019 Biodiversity Intactness Index/final-abund-bii-isl-main.tif')
bii_rich <- rast('data/Sanchez-Ortiz et al. 2019 Biodiversity Intactness Index/final-rich-bii-isl-main.tif')

bii_abund <- terra::aggregate(bii_abund, fact = 100, na.rm = T, fun = mean)
bii_rich <- terra::aggregate(bii_rich, fact = 100, na.rm = T, fun = mean)

bii_abund <- project(bii_abund, crs(esri_countries))
bii_rich <- project(bii_rich, crs(esri_countries))

bii_abund_mean <- extract(bii_abund, vect(esri_countries), fun = mean, na.rm = T, touches = T)
bii_rich_mean <- extract(bii_rich, vect(esri_countries), fun = mean, na.rm = T, touches = T)

esri_countries$bii_abun <-  bii_abund_mean$final.abund.bii.isl.main
esri_countries$bii_rich <-  bii_rich_mean$final.rich.bii.isl.main

tm_shape(esri_countries) + 
  tm_fill(col = 'bii_abun', 
          style = 'cont')

tm_shape(esri_countries) + 
  tm_fill(col = 'bii_rich', 
          style = 'cont')

#### b) annual change in forest area

annual_forest_change <- read_csv('data/annual-change-forest-area.csv') %>% 
  group_by(Entity, Code) %>% 
  summarise(change_forest_area = mean(`Annual net change in forest area`, na.rm = T))

#### c) % deforestation

deforestation_area <- read_csv('data/deforestation-share-forest-area.csv') %>% 
  filter(!is.na(Code)) %>% 
  group_by(Entity, Code) %>% 
  summarise(deforestation_area = mean(`Deforestation as share of forest area`, na.rm = T))

forest_change_data <- full_join(annual_forest_change, deforestation_area)

#### d) % wetland loss

# read in data
wetland_loss <- rast('data/wetland_loss/grid_ncdf/ensemblemean/wetland_loss_1700-2020_ensemblemean_v10.nc')

# make calculations
wetland_loss_2020 <- (wetland_loss$`wetland_area_Time=2020` / wetland_loss$`wetland_area_Time=1700`)
wetland_loss_2020[wetland_loss$`wetland_area_Time=1700` < 1] <- NA
wetland_loss_2020[wetland_loss_2020 > 1] <- NA
wetland_loss_2020 <- (wetland_loss_2020-1)*100
plot(wetland_loss_2020)
# compare with figure 1 in Fluet-Chouinard et al. 2023

# reproject
wetland_loss_2020 <- project(wetland_loss_2020, crs(esri_countries))

# make extractions
wetland_loss_2020_mean <- extract(wetland_loss_2020, vect(esri_countries), fun = mean, na.rm = T, touches = T)

# add to country dataset
esri_countries$wetland_loss_2020_mean <-  wetland_loss_2020_mean$wetland_area_Time.2020

tm_shape(esri_countries) + 
  tm_fill(col = 'wetland_loss_2020_mean', 
          style = 'cont')

#### e) Modified-natural habitat classification

nmhs <- rast('data/WCMC_natural_modified_habitat_screening_layer/natural_modified_habitat_screening_layer.tif')

# reproject to higher spatial resolution for faster processing
nmhs <- terra::aggregate(nmhs, fact = 100, na.rm = T, fun = mean)

# reproject
nmhs <- project(nmhs, crs(esri_countries))

# make extractions
nmhs_mean <- extract(nmhs, vect(esri_countries), fun = mean, na.rm = T, touches = T)

# add to country dataset
esri_countries$natural_modified_habitat <-  nmhs_mean$natural_modified_habitat_screening_layer

tm_shape(esri_countries) + 
  tm_fill(col = 'natural_modified_habitat', 
          style = 'cont')


###### BIODIVERSITY CONSERVATION DATASETS ----

#### a) Protected area coverage
pa <- list.files('data/Protected area percentage World Bank World Development Indicators', full.names = T)

pa <- lapply(pa, read_csv)

pa_data <- pa[[1]]
pa_countries <- pa[[2]]
pa_meta <- pa[[3]]

# make summaries and take latest reporting year
pa_summarised <- pa_data %>% 
  pivot_longer(cols = `1960`:`2022`) %>% 
  filter(!is.na(value)) %>% 
  group_by(`Country Code`) %>% 
  nest() %>% 
  mutate(max_year = purrr::map(data, ~max(as.numeric(.$name)))) %>% 
  unnest(c(data, max_year)) %>% 
  filter(name == as.character(max_year)) %>% 
  dplyr::select(`Country Code`, `Country Name`, value) %>% 
  rename(pa_coverage = value)
  
###### WORLD BANK POVERTY AND INEQUALITY DATA API ----


#### PIP poverty and inequality data ----
wb_data <- pipr::get_stats()
wb_countries <- pipr::get_countries()

# take data
names(wb_data)

# headcount
# poverty_gap
# poverty_severity
# gini inequality index

wb_poverty <- wb_data %>% 
  select(country_name, country_code, year, 
         headcount, poverty_gap, gini) %>% 
  # take most recent year
  group_by(country_code) %>% 
  filter(year == year[which.max(year)])

#### WB multidimensional poverty index ----

mdph <- readxl::read_xlsx('data/MPM-Data-SM23.xlsx', skip = 2) %>% 
  select(Code, Economy, `Multidimensional poverty headcount ratio (%)`) %>% 
  rename(multidimensional_poverty_headcount = `Multidimensional poverty headcount ratio (%)`, 
         country_code = Code, 
         country_name = Economy)

#### join together world bank data ----

# use full join to observe missing data in either dataset
wb_full <- full_join(wb_poverty, mdph)

wb_full <- wb_full %>% filter(!is.na(country_name))


#### COMBINE BIODIVERSITY DATA WITH PIP PIP ----

# read in ISO-3166 country codes from https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/tree/master/all
iso_codes <- read_csv('data/ISO-3166-COUNTRY-CODES.csv')
iso_codes <- iso_codes %>% select(name, `alpha-2`, `alpha-3`, region, `sub-region`)

#### a) IUCN species richness combined
esri_countries$IUCN_combined_SR
#### b) Sabatini et al. 2022 Plant alpha diversity
esri_countries$plant_alpha_SR
#### c) Theodoridis et al. 2020 Genetic diversity mammals
esri_countries$mammal_gd
#### a) Sanchez-Ortiz et al. 2019 BII glo
esri_countries$bii_abun
esri_countries$bii_rich
#### d) % wetland loss
esri_countries$wetland_loss_2020_mean
#### e) Modified-natural habitat classification
esri_countries$natural_modified_habitat

# make non-spatial to perform joins and merges, and then merge back in geomteries
esri_countries_nonSF <- st_drop_geometry(esri_countries)

# join together spatial data available with the iso codes
esri_countries_nonSF <- left_join(esri_countries_nonSF, iso_codes, by = c('ISO' = 'alpha-2'))

# forest together
#### b) annual change in forest area
#### c) % deforestation
esri_countries_nonSF <- left_join(esri_countries_nonSF, forest_change_data %>% ungroup() %>% select(-Entity), by = c('alpha-3' = 'Code'))

# protected area coverage
esri_countries_nonSF <- left_join(esri_countries_nonSF, pa_summarised %>% select(-`Country Name`), by = c('alpha-3' = 'Country Code'))


#### PIP poverty
#### PIP inequality
#### WB multidimensional poverty headcount
esri_countries_nonSF <- left_join(esri_countries_nonSF, 
                                wb_full %>% ungroup() %>% select(-country_name, -year), 
                                by = c('alpha-3' = 'country_code'))

final_country_data <- st_as_sf(left_join(esri_countries_nonSF, esri_countries %>% select(ISO), by = c('ISO' = 'ISO')))

#### SAVE COMBINED DATASET ----

#organise column names
final_country_data <- final_country_data %>% 
  select(`alpha-3`, COUNTRY, region, 
         IUCN_combined_SR, plant_alpha_SR, mammal_gd, bii_abun, bii_rich, 
         wetland_loss_2020_mean, natural_modified_habitat, change_forest_area, deforestation_area, 
         pa_coverage, headcount, poverty_gap, gini, multidimensional_poverty_headcount, geometry)

# check plots still make sense after manipulations
tm_shape(final_country_data) + 
  tm_fill(col = 'plant_alpha_SR', 
          style = 'cont')


dir.create('data/country_poverty_biodiversity')
sf::write_sf(st_as_sf(final_country_data), 'data/country_poverty_biodiversity/country_poverty_biodiversity.shp')
sf::write_sf(st_as_sf(final_country_data), 'data/country_poverty_biodiversity/country_poverty_biodiversity_names_3.csv')


