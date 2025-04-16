#### SEM overview plot

sem_effects <- list.files('figures/SEM_analysis/effect_tables/', full.names = T, pattern = '.RDS')

all_effects <- lapply(sem_effects, readRDS) %>% bind_rows()

all_effects <- all_effects %>%
  mutate(response = factor(response, levels = c("prehistory.states", "colonisation", "governance.strength", "debt", 'log.resource.rent',
                                                "poverty.rate", "national.poverty.headcount", "MD.poverty", "gini")))


all_effects <- all_effects %>% mutate(response  = recode(response ,
                                                     'range_richness' = 'species \n richness',
                                                     'log.resource.rent' = 'natural \n resource rent',
                                                     'prehistory.states' = 'pre-colonial \n state', 
                                                     'governance.strength'  = 'governance \n strength', 
                                                     'poverty.rate' = 'poverty rate', 
                                                     'national.poverty.headcount' = 'national \n poverty headcount'), 
                                      predictor = recode(predictor, 
                                                         'mammal.gd' = 'MGD', 
                                                         'range.richness' = 'SRR', 
                                                         'plant.richness' = 'PPR'))

all_effects <- all_effects %>% 
  mutate(sig = case_when(lower_ci > 0 & upper_ci > 0 ~ T, 
                         lower_ci < 0 & upper_ci < 0 ~ T, 
                         T ~ F)) %>% 
  mutate(across(c(effect, lower_ci, upper_ci), ~ case_when(
    predictor == 'latitude' ~ .x*-1,
    TRUE ~ .x
  )))

# filter to poverty response variables

mean_effects <- all_effects %>% 
  group_by(response, effect_type, predictor) %>% 
  summarise(across(c(effect, lower_ci, upper_ci), mean)) %>% 
  filter(effect_type != 'total') %>% 
  mutate(across(c(effect, lower_ci, upper_ci), ~ case_when(
    predictor == 'latitude' ~ .x * -1,
    TRUE ~ .x
  )))




#### Effect plot for full model ----

png('figures/SEM_analysis/effect_tables/effect_plots_full_models_tall.png', res = 600, width = 2000, height = 4000)
ggplot(data = all_effects %>% 
         filter(effect_type != 'total', 
                grepl('full_model_full|latOnly_full', model_name))) + 
  facet_wrap(~response, nrow = 4, dir = "v", scales = 'free_x') + 
  ggbeeswarm::geom_quasirandom(aes(y = predictor, x = effect, fill = effect_type, col = effect_type, pch = sig), 
                               dodge.width = 0.5, size = 2) +
  theme_bw() +
  geom_vline(aes(xintercept = 0), lwd = 1) + 
  scale_shape_manual(values = c(19, 1)) + 
  scale_colour_manual(values = c("#E59400", "#8F7CA1"))+ 
  scale_fill_manual(values = c("#E59400", "#8F7CA1")) + 
  theme(aspect.ratio = 0.5, 
        legend.position = 'none', 
        strip.background = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab(NULL) + 
  scale_y_discrete(position = "right")
dev.off()

### Effect plot for biodiversity only model, removing latitude, and backward selected ----

png('figures/SEM_analysis/effect_tables/effect_plots_backward_models_tall.png', 
    res = 400, width = 1500*1.2, height = 3000*1.2)
ggplot(data = all_effects %>% 
         filter(effect_type != 'total', 
                grepl('backward', model_name), 
                grepl('_noLat_|latOnly', model_name))) + 
  ggbeeswarm::geom_quasirandom(aes(y = predictor, x = effect, col = effect_type, fill = effect_type, pch = sig), 
                               dodge.width = 0.5, size = 2) +
  facet_wrap(~response, nrow = 4, dir = "v", scales = 'free') + 
  theme_bw() +
  geom_vline(aes(xintercept = 0), lwd = 1) + 
  scale_shape_manual(values = c(21, 19)) + 
  scale_colour_manual(values = c("#E59400", "#8F7CA1"))+ 
  scale_fill_manual(values = c("#E59400", "#8F7CA1")) + 
  theme(aspect.ratio = 0.5, 
        legend.position = 'none', 
        strip.background = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.spacing = unit(3, "lines")) + 
  ylab(NULL)
dev.off()
