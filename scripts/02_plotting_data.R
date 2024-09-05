#### Load packages and read in data ----

library(tidyverse)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(terra)
library(ggh4x)
library(mclust)

# read in both datasets
cpb <- read_sf('data/country_poverty_biodiversity/country_poverty_biodiversity.shp')
cpb_names <- names(read_sf('data/country_poverty_biodiversity/country_poverty_biodiversity_names_3.csv'))

# check name orderings match
View(cbind(names(cpb)[1:length(cpb_names)], cpb_names))

# convert names 
names(cpb)[1:length(cpb_names)] <- cpb_names

#### Convert to non-spatial and make combined dot-plot ----

# check through missing data
cpb %>% 
  st_drop_geometry() %>% 
  rowwise() %>%
  mutate(Count_NA = sum(is.na(cur_data()))) %>%
  ungroup %>% 
  arrange(Count_NA) %>% 
  View

# gather biodiversity data
cpd_long <- left_join(cpb %>% 
  st_drop_geometry() %>% 
  pivot_longer(cols = IUCN_combined_SR:pa_coverage, names_to = 'bio_metric', values_to = 'bio_value') %>% 
  select(COUNTRY, region, bio_metric, bio_value), 
  cpb %>% 
    st_drop_geometry() %>% 
    pivot_longer(cols = headcount:multidimensional_poverty_headcount, names_to = 'pov_metric', values_to = 'pov_value') %>% 
    select(COUNTRY, region, pov_metric, pov_value)
  )

# relabel factors
cpd_long$pov_metric <- factor(cpd_long$pov_metric, 
                              levels = c('headcount', 'poverty_gap', 'multidimensional_poverty_headcount', 'gini'), 
                              labels = c('poverty rate', 'poverty gap', 'MD poverty', 'inequality'))

cpd_long$bio_metric <- factor(cpd_long$bio_metric, 
                              levels = c("IUCN_combined_SR", "plant_alpha_SR", "mammal_gd", "bii_abun", "bii_rich", 
                                         "wetland_loss_2020_mean", "natural_modified_habitat", "change_forest_area", "deforestation_area", 
                                         'pa_coverage'),
                              labels = c('species range richness', 'plant plot richness', 'mammal genetic diversity', 'BII abundance', 
                                         'BII richness', 'wetland loss %', 'natural-modified index', 'change forest area', 'deforested area', 
                                         'protected %'))

# define metric filters
biodiv_metrics <- c('species range richness', 'plant plot richness', 'mammal genetic diversity')
bioloss_metrics <- c('BII abundance', 'BII richness')
habloss_metrics <- c('wetland loss %', 'natural-modified index')
con_metrics <- c('protected %')



# biodiversity
png('figures/biodiv_metrics.png', width = 2500, height = 2500, res = 300)
ggplot(data = cpd_long %>% filter(bio_metric %in% biodiv_metrics), 
       aes(x = bio_value, y = pov_value)) + 
  geom_point(aes(col = region), size = 3, alpha = 0.5, stroke = 0) + 
  stat_smooth(aes(col = region), method = 'lm', se = F) + 
  stat_smooth(col = 'black', method = 'lm', se = F, size = 2) + 
  ggh4x::facet_grid2(pov_metric ~ bio_metric, scales = 'free', independent = 'all') +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        strip.background = element_blank(), 
        legend.position = 'bottom', 
        aspect.ratio = 1) + 
  xlab('biodiversity dimension') + 
  ylab('poverty dimension')
dev.off()


# biodiversity change
png('figures/bioloss_metrics.png', width = 2500, height = 2500, res = 300)
ggplot(data = cpd_long %>% filter(bio_metric %in% bioloss_metrics), 
       aes(x = bio_value, y = pov_value, col = region)) + 
  geom_point(aes(col = region), size = 3, alpha = 0.5, stroke = 0) + 
  stat_smooth(method = 'lm', se = F) + 
  stat_smooth(col = 'black', method = 'lm', se = F, size = 2) + 
  ggh4x::facet_grid2(pov_metric ~ bio_metric, scales = 'free', independent = 'all') +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        strip.background = element_blank(), 
        legend.position = 'bottom', 
        aspect.ratio = 1) + 
  xlab('biodiversity dimension') + 
  ylab('poverty dimension')
dev.off()

# habitat change
png('figures/habitatloss_metrics.png', width = 2500, height = 2500, res = 300)
ggplot(data = cpd_long %>% filter(bio_metric %in% habloss_metrics), 
       aes(x = bio_value, y = pov_value, col = region)) + 
  geom_point(aes(col = region), size = 3, alpha = 0.5, stroke = 0) + 
  stat_smooth(method = 'lm', se = F) + 
  stat_smooth(col = 'black', method = 'gam', se = F, size = 2) + 
  ggh4x::facet_grid2(pov_metric ~ bio_metric, scales = 'free', independent = 'all') +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        strip.background = element_blank(), 
        legend.position = 'bottom', 
        aspect.ratio = 1)  + 
  xlab('biodiversity dimension') + 
  ylab('poverty dimension')
dev.off()

# protected area
png('figures/con_metrics.png', width = 2500, height = 2500, res = 300)
ggplot(data = cpd_long %>% filter(bio_metric %in% con_metrics), 
       aes(x = bio_value, y = pov_value, col = region)) + 
  geom_point(aes(col = region), size = 3, alpha = 0.5, stroke = 0) + 
  stat_smooth(method = 'lm', se = F) + 
  stat_smooth(col = 'black', method = 'gam', se = F, size = 2) + 
  ggh4x::facet_grid2(pov_metric ~ bio_metric, scales = 'free', independent = 'all') +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        strip.background = element_blank(), 
        legend.position = 'bottom', 
        aspect.ratio = 1) +
  xlab('biodiversity dimension') + 
  ylab('poverty dimension')
dev.off()

#### plot raw maps ----

cpd_long$pov_metric <- factor(cpd_long$pov_metric, 
                              levels = c('headcount', 'poverty_gap', 'multidimensional_poverty_headcount', 'gini'), 
                              labels = c('poverty rate', 'poverty gap', 'MD poverty', 'inequality'))

cpd_long$bio_metric <- factor(cpd_long$bio_metric, 
                              levels = c("IUCN_combined_SR", "plant_alpha_SR", "mammal_gd", "bii_abun", "bii_rich", 
                                         "wetland_loss_2020_mean", "natural_modified_habitat", "change_forest_area", "deforestation_area", 
                                         'pa_coverage'),
                              labels = c('species range richness', 'plant plot richness', 'mammal genetic diversity', 'BII abundance', 
                                         'BII richness', 'wetland loss %', 'natural-modified index', 'change forest area', 'deforested area', 
                                         'protected %'))


# remove antarctica
cpb <- cpb %>% filter(COUNTRY != 'Antarctica')

## poverty maps
png('figures/poverty_maps.png', width = 2000, height = 4000, res = 300)
tm_shape(cpb) + 
  tm_fill(col = c('headcount', 'poverty_gap', 'multidimensional_poverty_headcount', 'gini'),
          title = c('poverty rate', 'poverty gap', 'MD poverty', 'inequality'),
          style = 'cont')
dev.off()

## biodiversity maps
range_rich <- tm_shape(cpb) + 
  tm_fill(col = 'IUCN_combined_SR', title = 'species range richness', style = 'cont')

plant_plot <- tm_shape(cpb) + 
  tm_fill(col = 'plant_alpha_SR', title = 'plant plot richness', style = 'cont')

mammal_gd <- tm_shape(cpb) + 
  tm_fill(col = 'mammal_gd', title = 'mammal genetic diversity', style = 'cont')

png('figures/diversity_maps.png', width = 2000, height = 3000, res = 300)
tmap_arrange(range_rich, plant_plot, mammal_gd, nrow = 3)
dev.off()


## biodiversity intactness maps
bii_abun_map <- tm_shape(cpb) + 
  tm_fill(col = 'bii_abun', title = 'BII abundance', style = 'cont')

bii_rich_map <- tm_shape(cpb) + 
  tm_fill(col = 'bii_rich', title = 'BII richness', style = 'cont')

png('figures/intactness_maps.png', width = 2000, height = 2000, res = 300)
tmap_arrange(bii_abun_map, bii_rich_map, nrow = 2)
dev.off()


## habitat loss maps
wetland_loss_map <- tm_shape(cpb) + 
  tm_fill(col = 'wetland_loss_2020_mean', title = 'wetland loss %', style = 'cont')

nmh_map <- tm_shape(cpb) + 
  tm_fill(col = 'natural_modified_habitat', title = 'natural-modified index', style = 'cont')

png('figures/habitat_loss_maps.png', width = 2000, height = 2000, res = 300)
tmap_arrange(wetland_loss_map, nmh_map, nrow = 2)
dev.off()


## protected area map
png('figures/protected_area_maps.png', width = 2000, height = 1000, res = 300)
tm_shape(cpb) + 
  tm_fill(col = 'pa_coverage', title = '% protected area', style = 'cont')
dev.off()


#### Create a rescaled map of biodiversity / poverty ----

new <- cpb %>% 
  mutate(across(c(IUCN_combined_SR, plant_alpha_SR, mammal_gd, 
                  headcount, poverty_gap, gini, multidimensional_poverty_headcount, pa_coverage), 
                na.rm = T,
                scales::rescale)) %>% 
  rowwise() %>%
  mutate(mean_bio = mean(c(IUCN_combined_SR, plant_alpha_SR, mammal_gd), na.rm = T), 
         mean_pov = mean(c(headcount, poverty_gap, gini, multidimensional_poverty_headcount), na.rm = T)) %>% 
  filter(!is.na(mean_pov), !is.na(mean_bio)) %>% 
  ungroup() %>% 
  mutate(across(c(mean_bio, mean_pov),
                na.last = NA,
                rank)) %>% 
  mutate(bio_pov = mean_pov - mean_bio, 
         pa_over = pa_coverage - bio_pov) %>% 
  filter(!is.na(mean_pov), !is.na(mean_bio)) %>% 
  mutate(pa_rank = rank(.$pa_coverage, na.last = NA))

new$bio_pov_resid <- as.numeric(residuals(lm(new$mean_pov ~ new$mean_bio)))

ggplot(data = new) + 
  geom_point(aes(x = mean_bio, y = mean_pov, size = bio_pov))

ggplot(data = new) + 
  geom_point(aes(x = mean_bio, y = mean_pov, col = bio_pov)) + 
  scale_color_gradient2(low = "red",
                        mid = "gray50",
                        high = "blue") + 
  stat_smooth(aes(x = mean_bio, y = mean_pov), method = 'lm') + 
  theme_bw() + 
  theme(panel.grid = element_blank())




png('figures/bio_pov.png', width = 2000, height = 2000, res = 300)
tm_shape(new) + 
  tm_fill(c('mean_pov', 'mean_bio'), title = c('poverty index', 'biodiversity index'), style = 'cont')
dev.off()

png('figures/bio_pov_contrast.png', width = 2000, height = 1000, res = 300)
tm_shape(new) + 
  tm_fill('bio_pov', style = 'cont', title = 'poverty - biodiversity', 
          palette = 'PRGn') + 
  tm_layout(legend.outside = T) + 
  tm_shape(new) + 
  tm_borders()
dev.off()

tmaptools::get_brewer_pal('RdYlGn')
png('figures/bio_pov_dotplot.png', width = 1000, height = 1000, res = 300)
ggplot(data = new) + 
  geom_point(aes(x = mean_bio, y = mean_pov, fill = bio_pov), size = 2, pch = 21) + 
  scale_fill_gradient2(low = "#5B1567",
                        mid = "white",
                        high = "#0D5D18") + 
  stat_smooth(aes(x = mean_bio, y = mean_pov), method = 'lm', col = 'black') + 
  theme_bw() + 
  theme(panel.grid = element_blank(), legend.position = 'none') + 
  ylab('poverty index') + 
  xlab('biodiversity index')
dev.off()

png('figures/pa_targets.png', width = 2000, height = 1000, res = 300)
tm_shape(new) + 
  tm_fill('pa_coverage', style = 'cont') + 
  tm_shape(new) + 
  tm_borders()
dev.off() 

tm_shape(new) + 
  tm_fill('pa_over', style = 'cont', palette = 'PRGn') + 
  tm_shape(new) + 
  tm_borders()

ggplot(data = new) + 
  geom_point(aes(x = bio_pov, y = pa_coverage), size = 2, pch = 21) + 
  scale_fill_gradient2(low = "#5B1567",
                       mid = "white",
                       high = "#0D5D18") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), legend.position = 'none') + 
  ylab('poverty index') + 
  xlab('biodiversity index')



tm_shape(new) + 
  tm_fill('bio_pov_resid', style = 'cont') + 
  tm_shape(new) + 
  tm_borders()


### ranking plots
new_rank <- pivot_longer(new, cols = c('mean_pov', 'mean_bio', 'pa_rank')) %>% 
  select(COUNTRY, region, name, value)

library(GGally)
png('figures/bio_pov_con_parcoord.png', width = 3000, height = 1000, res = 300)
GGally::ggparcoord(data = new, 
                   columns =  which(names(new) %in% c('mean_pov', 'mean_bio', 'pa_rank')), 
                   groupColumn = which(names(new) %in% c('region')),
                   scale = 'globalminmax', 
                   showPoints = T) + 
  theme_bw() + 
  facet_wrap(~region, nrow = 1) + 
  theme(panel.grid = element_blank(), 
        aspect.ratio = 0.75, 
        legend.position = 'none', 
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab(NULL) + 
  ylab('rank') + 
  scale_x_discrete(labels = c('biodiversity', 'poverty', 'conservation'))
dev.off()  


png('figures/bio_pov_con_parcoord_long.png', width = 1000, height = 3000, res = 300)
GGally::ggparcoord(data = new, 
                   columns =  which(names(new) %in% c('mean_pov', 'mean_bio', 'pa_rank')), 
                   groupColumn = which(names(new) %in% c('region')),
                   scale = 'globalminmax', 
                   showPoints = T) + 
  theme_bw() + 
  facet_wrap(~region, ncol = 1) + 
  theme(panel.grid = element_blank(), 
        aspect.ratio = 0.75, 
        legend.position = 'none', 
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab(NULL) + 
  ylab('rank') + 
  scale_x_discrete(labels = c('biodiversity', 'poverty', 'conservation'))
dev.off()  


#### classification scheme ----

scheme <- cpb %>% 
  mutate(across(c(IUCN_combined_SR, plant_alpha_SR, mammal_gd, 
                  headcount, poverty_gap, gini, multidimensional_poverty_headcount, pa_coverage), 
                na.rm = T,
                scales::rescale)) %>% 
  rowwise() %>%
  mutate(mean_bio = mean(c(IUCN_combined_SR, plant_alpha_SR, mammal_gd), na.rm = T), 
         mean_pov = mean(c(headcount, poverty_gap, gini, multidimensional_poverty_headcount), na.rm = T)) %>% 
  select(COUNTRY, region, mean_bio, mean_pov, pa_coverage) %>% 
   filter(!is.na(mean_pov), !is.na(mean_bio), !is.na(pa_coverage)) %>% 
  ungroup() %>% 
  mutate(across(c(mean_bio, mean_pov, pa_coverage),
                na.last = NA,
                rank)) %>% 
  mutate(bio_quantile = ifelse(quantile(.$mean_bio, 0.5, na.rm = T)    < .$mean_bio, 'high bio', 'low bio'), 
         pov_quantile = ifelse(quantile(.$mean_pov, 0.5)    < .$mean_pov, 'high pov', 'low pov'), 
         pa_quantile  = ifelse(quantile(.$pa_coverage, 0.5) < .$pa_coverage, 'high pa', 'low pa'), 
         scheme = paste(bio_quantile, pov_quantile, pa_quantile))


# table the different types of scheme
table(scheme$scheme)

# table the different classifications by continents
cbind(table(scheme$scheme, scheme$region), rowSums(table(scheme$scheme, scheme$region)))

scheme$scheme_1 <-ifelse(scheme$scheme == 'high bio high pov high pa', 
                         'pattern 1', )

# define region names
region_names <- c(
  `1` = "Africa",
  `2` = "Americas", 
  `3` = "Asia", 
  `4` = "Europe", 
  `5` = "Oceania")

# define scheme names 
scheme_names <- c('HB-HP-HC', 'HB-HP-LC', 
                  'HB-LP-HC', 'HB-LP-LC', 
                  'LB-HP-HC', 'LB-HP-LC', 
                  'LB-LP-HC', 'LB-LP-LC')
names(scheme_names) <- sort(unique(scheme$scheme))

brewer.pal(8, 'Set1')
cols <-  c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", 
           "#FF7F00", "Gold", "#A65628", "#F781BF")
# gally plot small
png(filename = 'figures/gally_region_scheme_small.png', res = 300, width = 3000, height = 2000)
GGally::ggparcoord(data = scheme, 
                   columns =  which(names(scheme) %in% c('mean_pov', 'mean_bio', 'pa_coverage')), 
                   groupColumn = 'scheme',
                   scale = 'globalminmax', 
                   showPoints = T) + 
  theme_bw() + 
  facet_grid(~region, 
             labeller = labeller(region = as_labeller(region_names),
                                 scheme = as_labeller(scheme_names))) + 
  theme_minimal() + 
  theme(#panel.grid = element_blank(), 
        aspect.ratio = 1, 
       #legend.position = 'none', 
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab(NULL) + 
  ylab('rank') + 
  scale_x_discrete(labels = c('biodiversity', 'poverty', 'conservation'))  + 
  scale_colour_manual(values = cols) 
dev.off()


# gally plot large
png(filename = 'figures/gally_region_scheme.png', res = 300, width = 3000, height = 2000)
GGally::ggparcoord(data = scheme, 
                   columns =  which(names(scheme) %in% c('mean_pov', 'mean_bio', 'pa_coverage')), 
                   groupColumn = 'scheme',
                   scale = 'globalminmax', 
                   showPoints = T) + 
  theme_minimal() + 
  theme(panel.grid = element_line(colour = 'gray90'), 
        axis.line = element_line(),
    aspect.ratio = 1, 
    legend.position = 'none', 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.spacing = unit(0.5, 'cm')) + 
  facet_grid(region ~ scheme, 
             labeller = labeller(region = as_labeller(region_names),
                                 scheme = as_labeller(scheme_names))) + 
  xlab(NULL) + 
  ylab('rank') + 
  scale_x_discrete(labels = c('biodiversity', 'poverty', 'conservation')) + 
  scale_colour_manual(values = cols)
dev.off()




#### Gaussian mixture model classifiction - all metrics ----

# Generate synthetic multivariate data
input_data <- cpb %>% select(COUNTRY, region, 
                             IUCN_combined_SR, plant_alpha_SR, mammal_gd, 
                             headcount, poverty_gap, gini, multidimensional_poverty_headcount, pa_coverage) %>% 
  mutate(across(c(IUCN_combined_SR, plant_alpha_SR, mammal_gd, 
                  headcount, poverty_gap, gini, multidimensional_poverty_headcount, pa_coverage), 
                na.rm = T,
                scales::rescale)) %>% 
  st_drop_geometry() %>% 
  na.omit() 

# create simpler data structure
X_1 <- input_data %>% 
  select(IUCN_combined_SR, plant_alpha_SR, mammal_gd, 
         headcount, poverty_gap, gini, multidimensional_poverty_headcount, pa_coverage) %>% 
  data.frame()

# estaimte baysian information criteria across clusters and model types
BIC <- mclustBIC(X_1)
plot(BIC)
summary(BIC)

# Fit optimal model and plot classifications and uncertainty
mod1 <- Mclust(X_1, x = BIC)
summary(mod1, parameters = TRUE)
plot(mod1, what = "classification")
table(as.vector(input_data[['region']]), mod1$classification)
plot(mod1, what = "uncertainty")
factoextra::fviz_mclust(mod1, what = 'classification')

input_data$all_metric_class <- as.character(mod1$classification)
input_data$all_metric_uncer <- mod1$uncertainty
input_data$all_metric_uncer_rescale <- scales::rescale((input_data$all_metric_uncer+0.0001))

#### Gaussian mixture model classifiction - summarised metrics ----


X_2 <- input_data %>% 
  rowwise() %>%
  mutate(mean_bio = mean(c(IUCN_combined_SR, plant_alpha_SR, mammal_gd), na.rm = T), 
         mean_pov = mean(c(headcount, poverty_gap, gini, multidimensional_poverty_headcount), na.rm = T)) %>% 
  select(COUNTRY, region, mean_bio, mean_pov, pa_coverage) %>% 
  select(mean_bio, mean_pov, pa_coverage) %>% 
  data.frame()

# estaimte baysian information criteria across clusters and model types
BIC <- mclustBIC(X_2)
plot(BIC)
summary(BIC)

# Fit optimal model and plot classifications and uncertainty
mod2 <- Mclust(X_2, x = BIC, G = 4)
summary(mod2, parameters = TRUE)
png('figures/GMM-pairs-classification.png', res = 300, width = 2000, height = 2000)
plot(mod2, what = "classification")
dev.off()
table(as.vector(input_data[['region']]), mod2$classification)
plot(mod2, what = "uncertainty")
factoextra::fviz_mclust(mod2, what = 'classification')

input_data$mean_metric_class <- as.character(mod2$classification)
input_data$mean_metric_uncer <- mod2$uncertainty

table(input_data$all_metric_class, 
      input_data$mean_metric_class)


#### plot new classifications with ggally ----

input_data_2 <- cbind(X_2[,1:2], input_data)

input_data_2 <- input_data_2 %>% mutate(across(c(mean_bio, mean_pov, pa_coverage),
                               na.last = NA,
                               rank)) 


input_data_2 <- input_data_2 %>% 
  mutate(bio_quantile = ifelse(quantile(.$mean_bio, 0.5, na.rm = T)    < .$mean_bio, 'high bio', 'low bio'), 
         pov_quantile = ifelse(quantile(.$mean_pov, 0.5)    < .$mean_pov, 'high pov', 'low pov'), 
         pa_quantile  = ifelse(quantile(.$pa_coverage, 0.5) < .$pa_coverage, 'high pa', 'low pa'), 
         scheme       = paste(bio_quantile, pov_quantile, pa_quantile))


# table the different types of scheme
table(input_data_2$scheme)

# table the different classifications by continents
table(input_data_2$scheme, input_data_2$mean_metric_class)
chisq.test(table(input_data_2$scheme, input_data_2$mean_metric_class))


# define region names
region_names <- c(
  `1` = "Africa",
  `2` = "Americas", 
  `3` = "Asia", 
  `4` = "Europe", 
  `5` = "Oceania")

# define scheme names 
scheme_names <- c('HB-HP-HC', 'HB-HP-LC', 
                  'HB-LP-HC', 'HB-LP-LC', 
                  'LB-HP-HC', 'LB-HP-LC', 
                  'LB-LP-HC', 'LB-LP-LC')
names(scheme_names) <- sort(unique(scheme$scheme))

# define scheme names 
#scheme_names <- as.character(seq_along(unique(input_data_2$mean_metric_class)))
#names(scheme_names) <- sort(unique(scheme_names))


# pivot wider based on simple columns
test_plot <- input_data_2 %>% 
  pivot_longer(cols = c(mean_bio, mean_pov, pa_coverage))

png(filename = 'figures/gally_region_scheme_GausMixMod.png', res = 300, width = 3000, height = 2000)
ggplot(data = test_plot %>% arrange(COUNTRY)) + 
  geom_point(aes(x = name, 
                 y = value, 
                 size = 1/(mean_metric_uncer+0.01), 
                 alpha = 1/(mean_metric_uncer+0.01), 
                 col = mean_metric_class)) + 
  geom_line(aes(x = name, y = value, group = paste0(COUNTRY, region), alpha = 1/(mean_metric_uncer+0.01), 
                col = mean_metric_class)) + 
  facet_grid(region ~ scheme, 
             labeller = labeller(scheme = as_labeller(scheme_names))) + 
  scale_size_continuous(range = c(0.1, 2)) + 
  scale_alpha_continuous(range = c(0.5,1)) + 
  theme_minimal() + 
  theme(panel.grid = element_line(colour = 'gray90'), 
        axis.line = element_line(),
        aspect.ratio = 1, 
        legend.position = 'none', 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.spacing = unit(0.5, 'cm')) + 
  scale_x_discrete(labels = c('biodiversity', 'poverty', 'conservation')) + 
  scale_colour_manual(values = cols[1:4])
dev.off()


input_data_2 %>% select(COUNTRY, mean_bio, mean_pov, 
                        pa_coverage,  
                        bio_quantile, pov_quantile, pa_quantile, 
                        mean_metric_class) %>% unique() %>% 
  rename(., 
         biodiversity_rank = mean_bio, 
         poverty_rank = mean_pov, 
         protection_rank = pa_coverage, 
         statistical_class = mean_metric_class) %>% 
  write.csv(., file = 'data/B-P-C_table.csv', row.names = F)
