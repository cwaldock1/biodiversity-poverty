pacman::p_load(tidyverse)

# load in dataset shared by Ole

civ <- readxl::read_xlsx("data/global civilisations/civilizations and crops_final.xlsx", sheet = 1)

cleaned <- civ %>% 
  mutate(first_year_b2k = case_when(!is.na(first_year_BC) ~ first_year_BC+2000, 
                                    !is.na(first_year_AD) ~ 2000-first_year_AD),
         last_year_b2k  = case_when(!is.na(last_year_BC) ~ last_year_BC+2000, 
                                    !is.na(last_year_AD) ~ 2000-last_year_AD), 
         y = as.numeric(str_split(coordinates,',', simplify = T)[,1]),
         x = as.numeric(str_split(coordinates,',', simplify = T)[,2])) %>% 
  janitor::clean_names()


civ_sf <- st_transform(st_as_sf(cleaned, coords = c('x', 'y'), crs = 'wgs84'), crs = 'ESRI:54009')


library(rnaturalearth)
library(rnaturalearthdata)

target_crs <- st_crs("+proj=moll +lon_0=5 +x_0=0 +y_0=0 +lat_ts=45 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != 'Antarctica') %>% 
  st_transform(4326) %>%
  st_make_valid()

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

world_fixed <- world %>% st_difference(polygon)

world_fixed <- st_transform(world_fixed, target_crs)

ggplot(data = world_fixed) +
  geom_sf()


# tm_shape(world_fixed) + 
#  tm_borders(col = 'black') + 
#  tm_layout(legend.outside = T, 
#            frame = FALSE, bg.color = "transparent") + 
#  tm_shape(civ_sf) + 
#  tm_dots(col = 'first_year_b2k', 
#          style = 'cont',
#          size = 1, 
#          palette = 'viridis')

library(ggrepel)
pacman::p_load(devtools)
devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)

civ_sf <- st_as_sf(cleaned, coords = c('x', 'y'), crs = 'wgs84')

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != 'Antarctica') %>% 
  st_transform(4326) %>%
  st_make_valid()

civ_world_map <- ggplot(world) + 
  geom_sf(data = world, col = 'gray80', fill = 'gray70', inherit.aes = TRUE) + 
  geom_sf(data = civ_sf %>% arrange(desc(first_year_b2k)), aes(fill = first_year_b2k, size = first_year_b2k-last_year_b2k), 
          pch = 21) + 
  geom_sf_text_repel(data = civ_sf, aes(label = name), force = 0.1, seed = 3, size = 2.5, max.overlaps=200) + 
  theme_bw() + 
  theme(panel.border = element_blank()) + 
  scale_fill_viridis_c(option = 'C') + 
  theme(legend.position = 'bottom') + 
  labs(size = 'Duration', fill = 'Years B2K') +
  scale_y_continuous(breaks = c(-50, -25, 0, 25, 50), limits = c(-50, 70))

png('figures/figure1/world_civilisations.png', res = 300, height = 2000, width = 3000)
civ_world_map
dev.off()

# colour palette
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

png(filename = 'figures/figure1/world_civilisations_timeline.png', res = 300, width = 1000, height = 1000)
civ_timeline
dev.off()


### read in madison data for simpler regions and times
world_gdp <- readxl::read_xlsx("data/global civilisations/globalGDP_maddison.xlsx")

# quick plot of barplots
ggplot(data = world_gdp) + 
  geom_bar(aes(x = region, y = gdp), stat = 'identity') + 
  facet_wrap(~year, scales = 'free_y')

# reformat for plotting on single axis
world_gdp <- world_gdp %>% 
  pivot_wider(names_from = region, values_from = gdp) %>% 
  pivot_longer(3:7) %>% 
  mutate(perc_gdp =value/world, 
         region = str_to_sentence(name), 
         region = case_when(region == 'Europe' ~ 'West Europe', 
                            region == 'Usa' ~ 'USA', 
                            T ~ region))

# get mean latitude to join in
world %>% 
  filter(admin %in% c('China', 'India') | subregion %in% c('Western Europe', 'Northern Europe', 'Southern Europe', 'Northern America')) %>% 
  pull(admin) %>% 
  sort

world_latitudes <- world %>% 
  filter(admin %in% c('China', 'India') | 
           subregion %in% c('Western Europe', 'Northern Europe', 'Southern Europe', 'Northern America') | 
         region_un == 'Africa') %>% 
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
  mutate(coords = purrr::map(data, ~mean(st_coordinates(.)[,2], na.rm = T))) %>% 
  unnest(coords) %>% 
  select(-data)


# join in regional latitudes
world_gdp <- left_join(world_gdp, world_latitudes, by = c('region' = 'region_new'))
         
  
png(filename = 'figures/figure1/global_gdp_comparison.png', res = 300, width = 1000, height = 1000)
world_gdp_plot <- ggplot(data = world_gdp) + 
  geom_point(aes(x = year, y = perc_gdp*100, col = name), size = 5) +
  geom_line(aes(x = year, y = perc_gdp*100, col = name), lwd = 1) + 
  scale_colour_manual(values = viridis::inferno(5, begin = 0, end = 0.8)) + 
  theme_bw() + 
  ylab('Estimated % of global GDP') + 
  xlab('Years before present') + 
  theme(aspect.ratio = 1, 
        panel.grid = element_blank(), 
        legend.position = 'bottom') + 
  labs(colour = 'region')
world_gdp_plot
dev.off()

png(filename = 'figures/figure1/global_gdp_comparison_v2.png', res = 300, width = 1000, height = 1000)
world_gdp_plot_v2 <- 
  ggplot(data = world_gdp) + 
  geom_vline(aes(xintercept = 1000)) + 
  geom_point(aes(x = year, y = perc_gdp*100, col = coords, group = region), size = 5) +
  geom_line(aes(x = year, y = perc_gdp*100, col = coords, group = region), lwd = 1) + 
  #geom_text(data = world_gdp %>% filter(year == 1000), 
  #          aes(x = year+100+c(100,0,0,80,0), y = (perc_gdp*100)+c(-0,0.2,1.5,4.5,1), col = coords, label = region, 
  #              angle = c(-10, -10, 10, 15, 0)),
  #          lwd = 1, hjust = 0) + 
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


world_gdp_plot_v2
dev.off()

png(filename = 'figures/figure1/global_gdp_comparison_v3.png', res = 300, width = 1000, height = 1000)
world_gdp_plot_v3 <- ggplot(data = world_gdp %>% arrange(desc(region))) + 
  geom_vline(aes(xintercept = 1000)) + 
  geom_line(aes(x = year, y = coords, colour = name), lwd = 1) + 
  geom_point(aes(x = year, y = coords, size = perc_gdp*100, colour = name)) +
  geom_text(data = world_gdp %>% filter(year == 1000), 
            aes(x = year+c(100,100,100,100,100), y = coords+c(3,3,3,4,3), 
                                                           label = region), 
            hjust = 0) + 
  scale_colour_manual(values = viridis::inferno(5, begin = 0, end = 0.8), guide = "none") + 
  theme_bw() + 
  ylab('Latitue') + 
  xlab('Years before present') + 
  theme(aspect.ratio = 1, 
        panel.grid = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.position = 'bottom') + 
  labs(size = '% world economy') + 
  scale_size(range = c(0.1, 20), breaks = c(0, 2, 10, 20,40)) + 
  scale_y_continuous(breaks = c(-50, -25, 0, 25, 50), limits = c(-50, 70))
world_gdp_plot_v3
dev.off()

png(filename = 'figures/figure1/global_gdp_comparison_and_timeline.png', res = 300, width = 2500, height = 1500)
civ_timeline + theme(aspect.ratio = 0.5, 
                     legend.position = 'none') + world_gdp_plot_v2 + theme(aspect.ratio = 0.5)
dev.off()

png(filename = 'figures/figure1/global_gdp_comparison_and_timeline_v2.png', res = 300, width = 2500, height = 1500, 
    bg = 'transparent')
civ_timeline + theme(aspect.ratio = 0.5) + world_gdp_plot_v3
dev.off()

