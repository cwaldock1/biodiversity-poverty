# Script to produce set of insets in figure 4
# Main plot is biodiversity-poverty-conservation 

# Create directory

dir.create('figures/figure2', recursive = T)

# mcluster function helper script getting outputs from model

source('scripts/00_mcluster_helper.R')

#### Load packages and read in data ----

# Load in required packages

pacman::p_load(tidyverse, dplyr, tidyr, sf, tmap, terra, ggh4x, mclust, GGally, rworldxtra, broom, tidyr, summarytools, missForest)

# Load in country outlines

data(countriesHigh)

# Read in both datasets

cpb <- read_sf('data/country_poverty_biodiversity/country_poverty_biodiversity.shp')
cpb_names <- names(read_csv('data/country_poverty_biodiversity/country_poverty_biodiversity_names_3.csv'))

# Convert names 

names(cpb)[1:length(cpb_names)] <- cpb_names

# Remove antarctica permanantly

cpb <- cpb %>% filter(COUNTRY != 'Antarctica')

# Give namibia a region
cpb$region[cpb$COUNTRY == 'Namibia'] <- 'Africa'

# Clean names as is messy below

cpb <- janitor::clean_names(cpb)

# Generate scaled data

cpb$headcount_loglog <- log(log(cpb$headcount+1)+1)
cpb$multidimensional_poverty_headcount_log <- log(cpb$multidimensional_poverty_headcount+1)

# Filter to the focal variables in our work

cpb <- cpb %>% 
  
  # remove data that does not have a region
  filter(!is.na(region)) %>% 
  
  # select focal columns
  select(country, region,
         iucn_combined_sr, plant_alpha_sr, mammal_gd,
         headcount_loglog, mean_npl, gini, multidimensional_poverty_headcount_log, pa_coverage) %>% 
  
  # rescale variables between 0-1
  mutate(across(c(iucn_combined_sr, plant_alpha_sr, mammal_gd, 
                  headcount_loglog, mean_npl, gini, multidimensional_poverty_headcount_log), 
                na.rm = T,
                scales::rescale), 
         pa_coverage = pa_coverage / 100)

# Check data distributions

summarytools::dfSummary(cpb %>% st_drop_geometry())


#### Missforest to fill in gaps in data - especially importance and each dataset has important gaps so the complete dataset is avaiable for few countries ----

# Generate input for missforest

mf_input <-  cpb %>% select(-country) %>% st_drop_geometry() %>% mutate(region = as.factor(region))

# Count number of NAs per row that are being imputed

no_NAs <- apply(X = is.na(mf_input), MARGIN = 1, FUN = sum)

# Add column of identifier for NAs (to add back in uncertainty)

cpb$no_NAs <- no_NAs

# Run missforest on dataset

set.seed(123)
mf_cpb <- missForest(xmis = mf_input %>% data.frame,
                                 maxiter = 10000, 
                                 ntree = 1000,
                                 variablewise = T, 
                                 decreasing = F)

# What is the quality of the imputations?

mf_cpb$OOBerror
range(mf_cpb$OOBerror)
summary(mf_cpb$OOBerror)

# Note that region has no NAs and so helps with the imputations in general - we will not be good at picking outliers within regions due to this structuring

# Get the imputations for all axes

mf_cpb_ximp <- mf_cpb$ximp

# Remove region and rename columns

mf_cpb_ximp <- mf_cpb_ximp %>% 
  select(-region) %>% 
  rename_with(~str_c(.,'_imp'), .cols = everything())

# Combine back with full data

cpb <- cbind(cpb, mf_cpb_ximp)

#### Gaussian Mixure Models to classify Biodiversity-Poverty-Conservation axis ----

# Simplify for input to GMM

X_1 <- cpb %>% 
  select(iucn_combined_sr_imp:pa_coverage_imp) %>% 
  st_drop_geometry() %>% 
  data.frame()

# Estimate baysian information criteria across clusters and model types
# After a lot of simulations with the data ()
set.seed(123)
BIC <- mclustBIC(X_1)
plot(BIC)
summary(BIC)
# LRT <- mclustBootstrapLRT(X_1, modelName = "VVE")


#### Fit optimal model and plot classifications and uncertainty ----

# Fit model using mclust and optimal parameters defined in BIC

set.seed(123)
mod1 <- Mclust(X_1, x = BIC, G = 4)

# Summary of model

mod1_summary <- summary(mod1, parameters = F)

# Inspect the model groups and biplots of all variables

factoextra::fviz_mclust(mod1, what = 'classification')

# See how the model classifies across regions

table(as.vector(cpb[['region']]), mod1$classification)


#### Create clustering from model ----

# Run clustering on the GMM
set.seed(123)
mod1dr <- MclustDR(mod1)

mod1dr$std.basis
mod1dr$dir

# Plot how means of groups vary across variables

mean_clust <- clusterVars(dat = X_1, col_idx = 1:ncol(X_1), mod = mod1, G = mod1dr$G)

# Process outputs of clustering data to longer format

full_clust_data <- mean_clust$full_data %>% 
  select(-(Prob1:Prob4)) %>% 
  pivot_longer(cols = iucn_combined_sr_imp:pa_coverage_imp)

# Revalue factor variable names

mean_clust$table$Vars <-  factor(mean_clust$table$Vars, 
                                                    levels =  tolower(paste0(c("IUCN_combined_SR", "plant_alpha_SR", "mammal_gd" ,                            
                                                               "headcount_loglog", "mean_npl", "gini",                                 
                                                               "multidimensional_poverty_headcount_log", "pa_coverage"), '_imp')),
                                                    labels = c('species range richness', 'plant plot richness', 'mammal genetic diversity', 
                                                               'poverty rate', 'national poverty rate', 'inequality', 'MD poverty', 'protected %'))


full_clust_data$name <-  factor(full_clust_data$name, 
                                 levels =  tolower(paste0(c("IUCN_combined_SR", "plant_alpha_SR", "mammal_gd" ,                            
                                                            "headcount_loglog", "mean_npl", "gini",                                 
                                                            "multidimensional_poverty_headcount_log", "pa_coverage"), '_imp')),
                                 labels = c('species range richness', 'plant plot richness', 'mammal genetic diversity', 
                                            'poverty rate', 'national poverty rate', 'inequality', 'MD poverty', 'protected %'))


# add in metric type for faceting

cluster_table <- mean_clust$table

cluster_table <- cluster_table %>% 
  
  # set high level of facet split
  mutate(metric_type = case_when(Vars %in% c('species range richness', 
                                             'plant plot richness', 
                                             'mammal genetic diversity') ~ '1. biodiversity', 
                                 Vars %in% c('poverty rate', 
                                             'national poverty rate', 
                                             'inequality', 
                                             'MD poverty') ~ '2. poverty', 
                                 Vars %in% c('protected %') ~ '3. conservation',
                                 T ~ NA)) %>% 
  
  # second level of facet split
  mutate(Vars_2 = fct_recode(Vars, 'p1' = 'poverty rate', 
                             'p2' = 'national poverty rate', 
                             'p3' =  'inequality', 
                             'p4' = 'MD poverty', 
                             'c1' = 'protected %', 
                             'b1' = 'species range richness', 
                             'b2' = 'plant plot richness', 
                             'b3' = 'mammal genetic diversity'))

# add in metric type for faceting via join

full_clust_data_join <- left_join(full_clust_data, cluster_table %>% select(Vars, Vars_2, metric_type) %>% unique, by = c('name'='Vars'))

# plot means across metrics with facets split by metric types

png('figures/figure2/cluster_averages_V2.png', res = 300, width = 6000, height = 1000, bg = 'transparent')
ggplot(data = cluster_table, aes(x = Vars_2, y = mean, group = cluster, colour = as.character(cluster))) + 
  geom_violin(data = full_clust_data_join, aes(x = Vars_2, y = value, group = name, fill = as.character(cluster)), scale = 'width', alpha = 0.5) +
  geom_line(aes(colour = as.character(cluster)), size = 1.5)+
  expand_limits(y=0) +                        
  labs(y = "metric mean", x = '')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.title = element_blank(), 
        text = element_text(size = 30), 
        strip.text = element_blank(), 
        legend.position = 'none') + 
  scale_colour_manual(values = viridis::turbo(mod1dr$G, begin = 0.05, end = 0.95)) + 
  scale_fill_manual(values = viridis::turbo(mod1dr$G, begin = 0.05, end = 0.95)) + 
  facet_grid(. ~ as.character(cluster)+metric_type, scales = "free", space='free') 
dev.off()



#### Plot with gg-gally and compare across country groupings ----

# Calculate average biodiversity and poverty measures

cpb <- cpb %>%
  rowwise() %>%
  mutate(mean_bio = mean(c(iucn_combined_sr_imp, plant_alpha_sr_imp, mammal_gd_imp), na.rm = T), 
         mean_pov = mean(c(headcount_loglog_imp, mean_npl_imp, gini, multidimensional_poverty_headcount_log_imp), na.rm = T))


# Bind together results of GMM with input data

cpb <- cbind(cpb,
             gmm_class = as.character(mod1$classification), 
             gmm_uncer = mod1$uncertainty)

# Calculate quantiles and test if above median value for classification

cpb <- cpb %>% 
  mutate(bio_quantile = ifelse(.$mean_bio > 0.5, 'HB', 'LB'), 
         pov_quantile = ifelse(.$mean_pov > 0.5, 'HP', 'LP'), 
         pa_quantile  = ifelse(pa_coverage_imp > 0.17, 'HC', 'LC'),  # 0.3 due to bio goals
         scheme       = paste(bio_quantile, pov_quantile, pa_quantile))

# Table the different types of scheme

table(cpb$scheme)

# High-low protected area by region 

data.frame(table(cpb$pa_quantile, cpb$region)) %>% 
  group_by(Var2) %>% 
  do(Perc = round(.$Freq/sum(.$Freq), 2)) %>% 
  unnest(Perc)

# High-low poverty by region

data.frame(table(cpb$pov_quantile, cpb$region)) %>% 
  group_by(Var2) %>% 
  do(Perc = round(.$Freq/sum(.$Freq), 2)) %>% 
  unnest(Perc)

# High-low biodiversity by region

data.frame(table(cpb$bio_quantile, cpb$region)) %>% 
  group_by(Var2) %>% 
  do(Perc = round(.$Freq/sum(.$Freq), 2)) %>% 
  unnest(Perc)

# Table the difference classifications by the quantitative groupings

table(cpb$scheme, cpb$gmm_class)

# Test the correspondance between qualitative scheme and GMM classifications

chisq.test(table(cpb$scheme, cpb$gmm_class))

# Define region names

region_names <- c(
  `1` = "Africa",
  `2` = "Americas", 
  `3` = "Asia", 
  `4` = "Europe", 
  `5` = "Oceania"
  )

# Pivot wider based on simple columns

gally_plot_data <- cpb %>% 
  pivot_longer(cols = c(mean_bio, mean_pov, pa_coverage)) %>% 
  arrange(desc(gmm_uncer))

# Get the country level percentages

percentage_labels <- data.frame(table(cpb$scheme, cpb$region)) %>% 
  group_by(Var1) %>% 
  mutate(sum_scheme = sum(Freq)) %>% 
  ungroup() %>% 
  group_by(Var2) %>% 
  mutate(sum_region = sum(Freq)) %>% 
  ungroup() %>% 
  mutate(prop_scheme_per_region = round((Freq/sum_region)*100), 
         prop_region_across_scheme = round((Freq/sum_scheme)*100)) %>% 
  rename(., region = Var2, scheme = Var1)


# Plot gally plot - no longer used in manuscript

png(filename = 'figures/figure2/gally_region_scheme_GausMixMod.png', res = 600, width = 2000*2, height = 1500*2)
ggplot(data = gally_plot_data) + 
  geom_line(aes(x = name, 
                y = value, 
                group = paste0(country, region), 
                lty = ifelse(no_NAs != 0, 3,1),
                # alpha = 1/(gmm_uncer+0.1), 
                # size = 1/(gmm_uncer+0.1),
                col = gmm_class)) + 
  geom_point(aes(x = name, 
                 y = value, 
                 size = 1/(gmm_uncer+0.01), 
                 alpha = 1/(gmm_uncer+0.1), 
                 fill = gmm_class), pch = 21, stroke = 0.5) + 
  geom_text(data = percentage_labels, aes(x = 1, y = 1, label = prop_scheme_per_region), size = 3) + 
  geom_text(data = percentage_labels, aes(x = 3, y = 1, label = prop_region_across_scheme), size = 3) + 
  facet_grid(region ~ scheme) + # , labeller = labeller(scheme = as_labeller(scheme))) + 
  scale_size_continuous(range = c(0.1, 2)) + 
  scale_linewidth(range = c(0.01, 1)) + 
  scale_alpha_continuous(range = c(0.1,1)) + 
  theme_minimal() + 
  theme(panel.grid = element_line(colour = 'gray90'), 
        axis.line = element_line(),
        aspect.ratio = 1, 
        legend.position = 'none', 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.spacing = unit(0.5, 'cm')) + 
  scale_x_discrete(labels = c('biodiversity', 'poverty', 'conservation')) + 
  scale_colour_manual(values = viridis::turbo(mod1dr$G, begin = 0.05, end = 0.95)) + 
  scale_fill_manual(values = viridis::turbo(mod1dr$G, begin = 0.05, end = 0.95)) + 
  scale_linetype_binned()+
  xlab(NULL) + 
  ylab('aggregate index value') +
  ylim(y = c(0,1.1))
dev.off()


#### Look at scatter plot of bio and pov index against PA ----

# Format data to produce simple scatter plot indicating relationships between variables

cpb_scatter <- cpb %>% 
  st_drop_geometry() %>% 
  select(country, region, pa_coverage, mean_bio, mean_pov, gmm_class, gmm_uncer) %>% 
  pivot_longer(cols = c('mean_bio', 'mean_pov')) %>% 
  mutate(name = case_when(name == 'mean_bio' ~ 'biodiversity', 
                          name == 'mean_pov' ~ 'poverty'))

# Check relationships broadly using glm

pa_glm        <- glm(pa_coverage ~ region*mean_bio*mean_pov, data = cpb, family = binomial(link = 'logit'))
pa_glm_simple <- glm(pa_coverage ~ region + mean_bio + mean_pov, data = cpb, family = binomial(link = 'logit'))

# Summarise findings of glms 

broom::glance(pa_glm)
broom::tidy(pa_glm)

# Perform stepwise model

summary(step(pa_glm))

# view simpler model

broom::glance(pa_glm_simple)
broom::tidy(pa_glm_simple)

# Get the final GLM for plotting

pa_glm <- glm(pa_coverage ~ 1, data = cpb, family = binomial(link = 'logit'))

# Get confidence intervals (only if there is a valid relationship)
#aa <- broom::augment(pa_glm,  interval="confidence", type.predict = 'response')
#aa <- aa %>% pivot_longer(cols = c(mean_bio, mean_pov))

# Plots highlight paradox

png(filename = 'figures/figure2/scatterplot_biopov_vs_PA.png', res = 300, width = 2000, height = 1000)
ggplot(data = cpb_scatter) + 
  geom_point(aes(x = value, y = pa_coverage, col = gmm_class, size = gmm_uncer)) + 
  geom_linerange(aes(y = exp(pa_glm$coefficients), xmin = min(cpb_scatter$value), xmax = max(cpb_scatter$value)), lty = 2) + 
  facet_grid(name~region) +
  theme_bw() + 
  theme(aspect.ratio = 1, 
        strip.background = element_blank(),
        legend.position = 'none') + 
  scale_size(range = c(2,0.1)) + 
  scale_colour_manual(values = viridis::turbo(4, begin = 0.05, end = 0.95)) + 
  xlab('indices of poverty or biodiversity') + 
  ylab('% protected areas') + 
  scale_y_continuous(labels = c(0, 20, 40 ,60)) + 
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0,1))
dev.off()


#### Map of GMM classes ----

# Estimate area of countries and islands

cpb <- cpb %>% st_cast(., 'POLYGON', split = T, do_split = T) %>% mutate(area = as.numeric(st_area(.)))

# Seperate small and large

cpb_small <- cpb %>% mutate(area = as.numeric(st_area(.))) %>% filter(area < 10430*1000) %>% st_buffer(., 100000)
cpb_large <- cpb %>%  mutate(area = as.numeric(st_area(.))) %>%  filter(area > 10430*1000)

# Bind together buffered sizes

cpb_2 <- bind_rows(cpb_small, cpb_large)

# In one set of runs class 4 is small islands that do not visualise well - trying buffering these

cpb_class3 <- cpb %>% 
  filter(!st_is_empty(.)) %>% filter(gmm_class == 3) %>% 
  st_buffer(., 150000) %>% 
  st_make_valid() %>% 
  .[-58,] # this island causes a bug in the map

# Combine input

cpb_map_class <- bind_rows(cpb_2 %>% filter(!st_is_empty(.)) %>% filter(gmm_class != 3), cpb_class3)
cpb_map_class$gmm_uncer <- ecdf(cpb_map_class$gmm_uncer)(cpb_map_class$gmm_uncer)

# Transform

cpb_map_class <- st_transform(cpb_map_class, crs = 'ESRI:54009')

png(filename = 'figures/figure2/classification_GMM_map.png', res = 300, width = 2000, height = 2000)
ggplot(cpb_map_class) + 
  geom_sf(aes(fill = gmm_class, alpha = gmm_uncer),col = 'black', inherit.aes = TRUE) + 
  theme_bw() + 
  theme(panel.border = element_blank()) + 
  scale_fill_manual(values = viridis::turbo(mod1dr$G, begin = 0.05, end = 0.95)) + 
  scale_alpha_continuous(range=c(1,0.1))
dev.off() 

### Some extra code to look at directions of variation found in the groupings ----

# Summary of model clustering

summary(mod1dr)

# Get directions of variation

d <- mod1dr$dir
d <- data.frame(d)
d$class <- as.character(as.numeric(as.character(mod1dr$classification)))

png('figures/figure2/dir1_dir2.png', res = 300, width = 750, height = 750)
ggplot(data = d) + 
  geom_point(aes(x = Dir1, y = Dir2, col = class), size = 2) + 
  theme_bw() + 
  theme(legend.position = 'none', 
        aspect.ratio = 1) + 
  scale_colour_manual(values = viridis::turbo(mod1dr$G, begin = 0.05, end = 0.95)) + 
  xlab('Direction 1') + ylab('Direction 2')
dev.off()

png('figures/figure2/dir2_dir3.png', res = 300, width = 750, height = 750)
ggplot(data = d) + 
  geom_point(aes(x = Dir2, y = Dir3, col = class), size = 2) + 
  theme_bw() + 
  theme(legend.position = 'none', 
        aspect.ratio = 1) + 
  scale_colour_manual(values = viridis::turbo(mod1dr$G, begin = 0.05, end = 0.95)) + 
  xlab('Direction 2') + ylab('Direction 3')
dev.off()


#### Plot the variables that explain the directions ----


# Get wide version of variable contributions to directions

var_dir <- mod1dr$std.basis %>% 
  data.frame() %>% 
  mutate(variable = rownames(.)) %>% 
  pivot_longer(cols = colnames(mod1dr$std.basis)) %>% 
  mutate(value = signif(value, 2))

# Relabel
var_dir$variable <- factor(var_dir$variable, 
                              levels = c("iucn_combined_sr_imp", "plant_alpha_sr_imp", "mammal_gd_imp" ,                            
                                         "headcount_loglog_imp", "mean_npl_imp", "gini_imp",                                 
                                         "multidimensional_poverty_headcount_log_imp", "pa_coverage_imp"), 
                              labels = c('species range richness', 'plant plot richness', 'mammal genetic diversity', 
                                         'poverty rate', 'national poverty rate', 'inequality', 'MD poverty', 'protected %'))

# Rename to numberings

var_dir$name <- gsub('Dir', '', var_dir$name)

# Make figure of directions and variables

png('figures/figure2/dimension_contributions.png', width = 2000, height = 1000, res = 300)
ggplot(data = var_dir %>% 
         arrange(name)) + 
  geom_tile(aes(x = variable, y = name, fill = value)) + 
  geom_text(aes(x = variable, y = name, label = value)) + 
  scale_fill_gradient2(low = "#bd0f06",
                       mid = "gray90",
                       high = "#2200c9") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        text = element_text(size = 17), 
        plot.margin = margin(10, 10, 10, 50), 
        legend.title = element_blank()) + 
  xlab(NULL) + 
  ylab('Direction') + 
  ylim(rev(unique(var_dir$name)))
dev.off()
