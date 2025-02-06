# Script to create figure 3 showing global patterns in historic wealth

# Load packages and data ----

# Load in packages
# install first devtools::install_github("yutannihilation/ggsflabel")
pacman::p_load(tidyverse, rnaturalearth, rnaturalearthdata, ggrepel, devtools, ggsflabel, sf, patchwork)

# Load in dataset

civ <- readxl::read_xlsx("data/global civilisations/civilizations and crops_final.xlsx", sheet = 1)

# Process and clean data

cleaned <- civ %>% 
  mutate(first_year_b2k = case_when(!is.na(first_year_BC) ~ first_year_BC+2000, 
                                    !is.na(first_year_AD) ~ 2000-first_year_AD),
         last_year_b2k  = case_when(!is.na(last_year_BC) ~ last_year_BC+2000, 
                                    !is.na(last_year_AD) ~ 2000-last_year_AD), 
         y = as.numeric(str_split(coordinates,',', simplify = T)[,1]),
         x = as.numeric(str_split(coordinates,',', simplify = T)[,2])) %>% 
  janitor::clean_names()

# Convert to a spatial object

civ_sf <- st_transform(st_as_sf(cleaned, coords = c('x', 'y'), crs = 'wgs84'), crs = 'ESRI:54009')

# Load in global maps

# Set target CRS

target_crs <- st_crs("+proj=moll +lon_0=5 +x_0=0 +y_0=0 +lat_ts=45 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Get world and transform

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != 'Antarctica') %>% 
  st_transform(4326) %>%
  st_make_valid()

# Shift the projection

offset <- 180 - 5

polygon <- st_polygon(x = list(rbind(
  c(-0.0001 - offset, 90),
  c(0 - offset, 90),
  c(0 - offset, -90),
  c(-0.0001 - offset, -90),
  c(-0.0001 - offset, 90)
))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# Fix polygons

world_fixed <- world %>% st_difference(polygon)

# Back transform

world_fixed <- st_transform(world_fixed, target_crs)

# Test plot

ggplot(data = world_fixed) +
  geom_sf()

# Convert civilisations to sf object

civ_sf <- st_as_sf(cleaned, coords = c('x', 'y'), crs = 'wgs84')

# Plot with reprojection

civ_world_map <- ggplot(world_fixed) + 
  geom_sf(data = world_fixed, col = 'gray80', fill = 'gray70', inherit.aes = TRUE) + 
  geom_sf(data = civ_sf %>% arrange(desc(first_year_b2k)), aes(fill = first_year_b2k, size = first_year_b2k-last_year_b2k), 
          pch = 21) + 
  geom_sf_text_repel(data = civ_sf, aes(label = name), force = 0.1, seed = 3, size = 2.5, max.overlaps=200) + 
  theme_bw() + 
  theme(panel.border = element_blank()) + 
  scale_fill_viridis_c(option = 'C') + 
  theme(legend.position = 'bottom') + 
  labs(size = 'Duration', fill = 'Years B2K') + 
  xlab(NULL) + 
  ylab(NULL)#+
  #scale_y_continuous(breaks = c(-50, -25, 0, 25, 50), limits = c(-50, 70))

png('figures/figure1/world_civilisations.png', res = 300, height = 2000, width = 3000)
civ_world_map
dev.off()

# Reference for colour palette

viridis::inferno(5, begin = 0, end = 0.8)
viridis::turbo(4)
c(Europe, Asia, Americas, Africa)
c('#932667FF', '#DD513AFF', '#1AE4B6FF', '#000004FF')


# plot of civilisatons and a timeline
civ_timeline <- ggplot(data = civ_sf %>% 
                         bind_cols(., st_coordinates(st_transform(civ_sf, crs = 'wgs84'))) %>% 
                         arrange(desc(region))) + 
  geom_text(data = data.frame(y=c(50, 35, 23, -5), 
                       x=c(3000, 6000, 5000, 5000), 
                       region = c('Europe', 'Asia', 'Africa', 'Americas')), aes(x = x, y = y, col = region, label = region)) +
  geom_linerange(aes(xmin = first_year_b2k, xmax = last_year_b2k, y = Y, col = region), lwd = 1) + 
  scale_x_reverse() +
 # scale_colour_manual(values = viridis::inferno(4, begin = 0, end = 0.8)[1:4]) +
  scale_colour_manual(values = c('#932667FF', '#DD513AFF', '#1AE4B6FF', '#000004FF'), 
                      breaks = c('Europe', 'Asia', 'Americas', 'Africa')) +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.line.y = element_blank(), 
        legend.position = 'bottom') + 
  scale_y_continuous(breaks = c(-50, -25, 0, 25, 50), limits = c(-50, 70)) + 
  geom_vline(aes(xintercept = 1000)) + 
  xlab('Years before present') + 
  ylab('Latitude')
civ_timeline

# Save civilisation timeline png

png(filename = 'figures/figure1/world_civilisations_timeline.png', res = 300, width = 1000, height = 1000)
civ_timeline
dev.off()


# Read in madison data for simpler regions and times

world_gdp <- readxl::read_xlsx("data/global civilisations/globalGDP_maddison.xlsx")

# Quick plot of barplots

ggplot(data = world_gdp) + 
  geom_bar(aes(x = region, y = gdp), stat = 'identity') + 
  facet_wrap(~year, scales = 'free_y')

# Reformat for plotting on single axis

world_gdp <- world_gdp %>% 
  pivot_wider(names_from = region, values_from = gdp) %>% 
  pivot_longer(3:7) %>% 
  mutate(perc_gdp =value/world, 
         region = str_to_sentence(name), 
         region = case_when(region == 'Europe' ~ 'West Europe', 
                            region == 'Usa' ~ 'USA', 
                            T ~ region))

# Get mean latitude for colouration

world_latitudes <- world %>% 
  filter(admin %in% c('China', 'India') | 
           subregion %in% c('Western Europe', 'Northern Europe', 'Southern Europe', 'Northern America') | 
         region_un == 'Africa') %>% 
  # rename countries or regions to fit data together
  mutate(region_new = case_when(admin == 'China' ~ 'China', 
                                admin == 'India' ~ 'India', 
                                subregion %in% c('Western Europe', 'Northern Europe', 'Southern Europe') ~ 'West Europe', 
                                admin == 'United States of America' ~ 'USA', 
                                region_un == 'Africa' ~ 'Africa', 
                                T ~ NA)) %>% 
  filter(!is.na(region_new)) %>% 
  group_by(region_new) %>% 
  summarize() %>% 
  group_by(region_new) %>% 
  nest() %>% 
  # get avearge of latitude
  mutate(coords = purrr::map(data, ~mean(st_coordinates(.)[,2], na.rm = T))) %>% 
  unnest(coords) %>% 
  select(-data)


# Join in regional latitudes

world_gdp <- left_join(world_gdp, world_latitudes, by = c('region' = 'region_new'))
         


# World map of GDP comparisons through time from 1000 to 2000AD

png(filename = 'figures/figure1/global_gdp_comparison_v2.png', res = 300, width = 1000, height = 1000)
world_gdp_plot_v2 <- 
  ggplot(data = world_gdp) + 
  geom_vline(aes(xintercept = 1000)) + 
  geom_point(aes(x = year, y = perc_gdp*100, col = coords, group = region), size = 5) +
  geom_line(aes(x = year, y = perc_gdp*100, col = coords, group = region), lwd = 1) + 
  scale_colour_gradient2(high = "#005AB5",
                         mid = "gray80",
                         low = "#DC3220", 
                         midpoint = 35) + 
  theme_bw() + 
  ylab('Estimated % of global GDP') + 
  xlab('Years before present') + 
  theme(aspect.ratio = 1, 
        panel.grid = element_blank(), 
        legend.position = 'bottom') + 
  labs(colour = 'latitude') 

png(filename = 'figures/figure1/global_gdp_comparison_and_timeline.png', res = 300, width = 2500, height = 1500)
civ_timeline + theme(aspect.ratio = 0.5, 
                     legend.position = 'none') + world_gdp_plot_v2 + theme(aspect.ratio = 0.5)
dev.off()





