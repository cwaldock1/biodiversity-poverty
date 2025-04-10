#### Read in mineral deposit data layer ----

# load in packages including those to perform spatial kernel analysis for testing
pacman::p_load(tidyverse,  # for general data management
               sf,         # for handling sf data class
               tmap,       # for mapping
               spatstat,   # A package with tools that underly the sparr package
               sparr,      # A package for estimating spatial intensity and relative risk
               raster, 
               mapview,
               maps, 
               tmap)     # The outputs of these KDE functions will be raster. This package gives us tools for working with rasters

# read in mineral data
minerals <- read_sf('data/mrds-trim/mrds-trim.shp')
minerals$DEV_STAT %>% unique
minerals$SITE_NAME %>% unique

# process mineral data to obtain gold, silver and diamonds
minerals <- minerals %>% 
  filter(grepl('AU|AG|GEM_D', CODE_LIST)) %>% 
  unique() %>% 
  mutate(AU = case_when(grepl('AU', CODE_LIST) ~ 1, T ~ 0), 
         AG = case_when(grepl('AG', CODE_LIST) ~ 1, T ~ 0), 
         GEM_D = case_when(grepl('GEM_D', CODE_LIST) ~ 1, T ~ 0)) 

# plot mineral distributions
minerals %>% filter(AU == 1) %>% mapview()
minerals %>% filter(AG == 1) %>% mapview()
minerals %>% filter(GEM_D == 1) %>% mapview()

# read in spatial data and define projection boundary for kde
install.packages("spatstat")
# read in arcGIS provided data layers for world map with 2 digit ISO codes
esri_countries <- esri_countries <- st_read('data/World_Countries_Generalized/World_Countries_Generalized.shp')
esri_countries <- esri_countries %>% st_union()
esri_countries <- esri_countries
esri_countries_bbox <- as.owin(esri_countries)


spatial_points <- coordinates(st_coordinates(minerals))
d_ppp <- ppp(x = spatial_points[,1], y = spatial_points[,2], esri_countries_bbox)
plot(coord_ppp)
h_os_d <- OS(d_ppp)
mineral_kde <- bivariate.density(pp = d_ppp, h0 = h_os_d, adapt = TRUE, edge = 'none')
mineral_raster <- rast(mineral_kde$z)

plot(mineral_raster)
plot(log10(mineral_raster+0.01))

my_density <- density(spatial_points[sample(1:nrow(spatial_points),100),], sigma = 100) 


#### Count number of point reco
minerals
esri_countries <- st_read('data/World_Countries_Generalized/World_Countries_Generalized.shp')
esri_countries <- st_as_sf(esri_countries)

joined <- st_join(minerals, esri_countries, left = F)


# group by polygonID and summarise
summary_df <- joined %>%
  group_by(FID) %>%
  summarise(
    point_count = n(),
    AU = sum(AU, na.rm = TRUE),
    AG = sum(AG, na.rm = TRUE),
    GEM_D = sum(GEM_D, na.rm = TRUE)
  )

# Join results back to polygon layer
countries_summary <- esri_countries %>%
  left_join(summary_df %>% st_drop_geometry(), by = "FID") %>% 
  mutate(across(c(point_count:GEM_D), function(x) case_when(is.na(x) ~ 0, T ~ x)),
         across(c(point_count:GEM_D), function(x) log10(x+1), .names = "{.col}_log10"))

# extract rasterized values of mineral resources from KDE
countries_summary$kde_point <- extract(mineral_raster, vect(countries_summary), mean)[,2] %>% sqrt %>% sqrt %>% sqrt
countries_summary_sf <- countries_summary %>% st_as_sf()
tmap_mode('view')
tm_shape(countries_summary_sf) + 
  tm_polygons(fill = 'AU_log10')
tm_shape(countries_summary_sf) + 
  tm_polygons(fill = 'AG_log10')
tm_shape(countries_summary_sf) + 
  tm_polygons(fill = 'GEM_D_log10')
tm_shape(countries_summary_sf) + 
  tm_polygons(fill = 'point_count_log10')
tm_shape(countries_summary_sf) + 
  tm_polygons(fill = 'kde_point')

# clean up mnmes and save object
clean_minerals <- st_drop_geometry(countries_summary_sf) %>% 
  janitor::clean_names() %>% 
  dplyr::select(country, iso, point_count:gem_d_log10)

# save 
write_csv(clean_minerals, file = 'data/mrds-trim/clean_minerals.csv')




