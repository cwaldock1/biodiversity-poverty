# Script to produce set of insets in figure 1
# Main plots are bi-plots and the underlying maps of these

#### Load packages and read in data ----

# Load in required packages

pacman::p_load(tidyverse, dplyr, tidyr, sf, tmap, terra, ggh4x, mclust, GGally, rworldxtra, broom, tidyr)

data(countriesHigh)

world_map <- rnaturalearth::ne_countries(scale = 110)
world_map <- world_map %>% filter(continent != 'Antarctica') %>% st_make_valid()

# Read in both datasets

cpb <- read_sf('data/country_poverty_biodiversity/country_poverty_biodiversity.shp')
cpb_names <- names(read_csv('data/country_poverty_biodiversity/country_poverty_biodiversity_names_3.csv'))

# Convert names 

names(cpb)[1:length(cpb_names)] <- cpb_names

# View amount of data per country

NA_count <- cpb %>% 
  st_drop_geometry() %>% 
  rowwise() %>%
  mutate(Count_NA = sum(is.na(cur_data()))) %>%
  ungroup %>% 
  arrange(Count_NA)

# What is the distribution of NAs

table(NA_count$Count_NA)
# 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 
# 87 17 49  9 12 10 12 19 15 13 11  3  4  7  5  3 

# Histogram of the distribution of NAs

hist(NA_count$Count_NA)

# Gather biodiversity data

cpd_long <- left_join(cpb %>% 
                        st_drop_geometry() %>% 
                        pivot_longer(cols = IUCN_combined_SR:pa_coverage, names_to = 'bio_metric', values_to = 'bio_value') %>% 
                        select(COUNTRY, region, bio_metric, bio_value), 
                      cpb %>% 
                        st_drop_geometry() %>% 
                        pivot_longer(cols = headcount:mean_npl, names_to = 'pov_metric', values_to = 'pov_value') %>% 
                        select(COUNTRY, region, pov_metric, pov_value),
                      by = c('COUNTRY', 'region'), 
                      relationship = "many-to-many")

# should be 50 comparisons per country
cpd_long %>% 
  group_by(COUNTRY) %>% 
  summarise(n = n()) %>% 
  filter(n > 50)

# relabel factors
cpd_long$pov_metric <- factor(cpd_long$pov_metric, 
                              levels = c('headcount', 'mean_npl', 'poverty_gap', 'multidimensional_poverty_headcount', 'gini'), 
                              labels = c('poverty rate', 'national poverty rate', 'poverty gap', 'MD poverty', 'inequality'))

cpd_long$bio_metric <- factor(cpd_long$bio_metric, 
                              levels = c("IUCN_combined_SR", "plant_alpha_SR", "mammal_gd", "bii_abun", "bii_rich", 
                                         "wetland_loss_2020_mean", "natural_modified_habitat", "change_forest_area", "deforestation_area", 
                                         'pa_coverage'),
                              labels = c('species range richness', 'plant plot richness', 'mammal genetic diversity', 'BII abundance', 
                                         'BII richness', 'wetland loss %', 'natural-modified index', 'change forest area', 'deforested area', 
                                         'protected %'))


#### Map: Country level variation in biodiversity and poverty ----

dir.create('figures/figure1/', recursive = T)

cpb_small <- cpb %>% mutate(area = as.numeric(st_area(.))) %>% filter(area < 10430*1000000) %>% st_buffer(., 100000)
cpb_large <- cpb %>% mutate(area = as.numeric(st_area(.))) %>%  filter(area > 10430*1000)

# bind together buffered sizes
cpb_2 <- bind_rows(cpb_small, cpb_large)
cpb <- cpb_2

## biodiversity maps
rich_map <- tm_shape(cpb %>% filter(COUNTRY != 'Antarctica', !is.na(IUCN_combined_SR))) + 
  tm_fill(col = 'IUCN_combined_SR', 
          title = 'species range richness', 
          style = 'cont', 
          palette = 'plasma', 
          legend.reverse = T) + 
  tm_shape(countriesHigh) + 
  tm_borders(col = 'black') + 
  tm_layout(legend.outside = T, 
            frame = FALSE, bg.color = "transparent")
# tmap_save(rich_map, filename = "figures/figure1/range_richness_map.eps", bg="transparent") # Note the eps extension for transparency in illustrator
tmap_save(rich_map, filename = "figures/figure1/range_richness_map.png", bg="transparent")

## poverty maps
headcount_map <- tm_shape(cpb %>% 
                            filter(COUNTRY != 'Antarctica', 
                                   COUNTRY != 'Tuvalu',
                                   !is.na(headcount)) %>% mutate(headcount = log(headcount+1))) + 
  tm_fill(col = 'headcount', 
          title = 'poverty rate', 
          style = 'cont', 
          palette = 'plasma', 
          legend.reverse = T) + 
  tm_shape(countriesHigh) + 
  tm_borders(col = 'black') + 
  tm_layout(legend.outside = T, 
            frame = FALSE, bg.color = "transparent")
# tmap_save(headcount_map, filename = "figures/figure1/poverty_headcount_map.eps", bg="transparent") # Note the eps extension for transparency in illustrator
tmap_save(headcount_map, filename = "figures/figure1/poverty_headcount_map.png", bg="transparent") # Note the eps extension for transparency in illustrator



#### Biplots of biodiversity and poverty relationships ---- 

# define metric filters
biodiv_metrics <- c('species range richness', 'plant plot richness', 'mammal genetic diversity')
bioloss_metrics <- c('BII abundance', 'BII richness')
habloss_metrics <- c('wetland loss %', 'natural-modified index')
con_metrics <- c('protected %')

# filter to plotable values
cpd_biplot <- cpd_long %>% filter(bio_metric %in% biodiv_metrics,
                    !is.na(bio_value),
                    !is.na(pov_value), 
                    pov_metric != 'poverty gap')


# calculate pairwise relationship between biometrics and poverty
biopov_labels <- cpd_biplot %>% 
  ungroup() %>% 
  group_by(bio_metric, pov_metric) %>% 
  nest() %>% 
  mutate(cor_test = purrr::map(data, ~cor.test(.$pov_value, .$bio_value, method = 'spearman')), 
         cor_output = purrr::map(cor_test, tidy),
         lm_test  = purrr::map(data, ~ lm(.$pov_value ~ .$bio_value)),
         lm_output = purrr::map(lm_test, glance)) %>% 
  unnest(cor_output, lm_output) %>% 
  mutate(estimate = signif(estimate, 2)) %>% 
  group_by(bio_metric, pov_metric) %>% 
  mutate(x_position = purrr::map(data, ~(max(.$bio_value) - min(.$bio_value))*0.3), 
         y_position = purrr::map(data, ~max(.$pov_value))) %>% 
  unnest(c(x_position, y_position)) %>% 
  # summarise p values
  mutate(p_value = case_when(p.value < 0.001 ~ '***',
                             p.value < 0.01 ~ '**',
                             p.value < 0.05 ~ '*', 
                             default = T ~ ' '))

# get summary estimates 
summary(biopov_labels$estimate)
summary(biopov_labels$r.squared)


# biodiversity
png('figures/figure1/biodiv_poverty_metrics.png', width = 2500, height = 2500, res = 300)
ggplot() + 
  geom_text(data = biopov_labels %>% arrange(bio_metric, pov_metric), aes(x = x_position, y = c(90, 90, 90, 70, 90, 90, 90, 70, 90, 90, 90, 70), label = paste0("\u03C1 = ", estimate, p_value)), size = 3) + 
  geom_point(data = cpd_biplot, 
             aes(col = region, x = bio_value, y = pov_value), 
             size = 3, alpha = 1, stroke = 0) + 
  stat_smooth(data = cpd_biplot,
              aes(x = bio_value, y = pov_value),
              col = 'black', 
              method = 'lm', se = F, size = 2) + 
#  ggh4x::facet_grid2(pov_metric ~ bio_metric, scales = 'free', independent = 'all') +
  facet_grid(pov_metric ~ bio_metric, scales = 'free') + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        strip.background = element_blank(), 
        legend.position = 'bottom', 
        aspect.ratio = 1, 
        text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_blank()) + 
  xlab('biodiversity dimension') + 
  ylab('poverty dimension') + 
  scale_colour_viridis_d(option = 'H')
dev.off()

cpd_biplot %>% 
  filter(bio_metric == 'mammal genetic diversity', pov_metric == 'inequality') %>% 
  select(bio_value, pov_value) %>% 
  ggplot(data = .) + stat_smooth(aes(x = pov_value, y = bio_value), method = 'lm')


#### Create aggregate plot of diversity and poverty ----


new <- cpb %>% 
  
  # remove all potential NAs - this removes many points
#  filter_at(vars(IUCN_combined_SR, plant_alpha_SR, mammal_gd, 
#                 headcount, mean_npl, gini, multidimensional_poverty_headcount),all_vars(!is.na(.))) %>% 
  
  # rescale values between 0-1
  mutate(across(c(IUCN_combined_SR, plant_alpha_SR, mammal_gd, 
                  headcount, mean_npl, gini, multidimensional_poverty_headcount, pa_coverage), 
                na.rm = T,
                scales::rescale)) %>% 
  
  rowwise() %>%
  
  # calculate average of biodiversity and poverty metrics together
  mutate(mean_bio = mean(c(IUCN_combined_SR, plant_alpha_SR, mammal_gd), na.rm = T), 
         mean_pov = mean(c(headcount, mean_npl, gini, multidimensional_poverty_headcount), na.rm = T)) %>% 
  filter(!is.na(mean_pov), !is.na(mean_bio)) %>% 
  ungroup() %>% 
  
  # find each countries ranking
#  mutate(across(c(mean_bio, mean_pov),
#                na.last = NA,
#                rank)) %>% 
  
  # find the difference of rankings
  mutate(pov_minus_bio = mean_pov - mean_bio, 
         pa_over = pa_coverage - pov_minus_bio, 
         pov_residual_bio = residuals(lm(mean_pov ~ mean_bio))) %>% 
  filter(!is.na(mean_pov), !is.na(mean_bio)) %>% 
  
  # rank the PA coverage
  mutate(pa_rank = rank(.$pa_coverage, na.last = NA))



# Biplot of ranked metrics
png('figures/figure1/biodiv_poverty_ranks.png', width = 2500/2, height = 2500/3, res = 300)
ggplot(data = new) + 
 # geom_vline(aes(xintercept = median(mean_bio))) +
 # geom_hline(aes(yintercept = median(mean_pov))) + 
  geom_point(aes(x = mean_bio, y = mean_pov, col = pov_residual_bio), size = 2, pch = 19, stroke = 0) + 
  scale_colour_gradient2(low = "#005AB5",
                       mid = "gray80",
                       high = "#DC3220") + 
  stat_smooth(aes(x = mean_bio, y = mean_pov), method = 'lm', col = 'black') + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        legend.position = 'none', 
        aspect.ratio = 1, 
        text = element_text(size = 12), 
        panel.border = element_blank(), 
        axis.line = element_line()) + 
  ylab('poverty index') + 
  xlab('biodiversity index') + 
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1))
dev.off()


# Map rank options
Mypal <- c('#005AB5','gray80','#DC3220')
biopov_rank_map <- tm_shape(new %>% filter(!is.na(pov_residual_bio))) + 
  tm_fill('pov_residual_bio', style = 'cont', title = 'residual poverty', 
          palette = Mypal, 
          legend.reverse = T, 
          midpoint = 0,
          colorNA = 'Black') + 
  tm_shape(countriesHigh) + 
  tm_borders(col = 'black') + 
  tm_layout(legend.outside = T, 
            frame = FALSE,
            bg.color = "transparent")
tmap_save(biopov_rank_map, filename = "figures/figure1/biopov_rank_map.png", bg="transparent") # Note the eps extension for transparency in illustrator


