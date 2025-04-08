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




png('figures/SEM_analysis/effect_tables/effect_plots_mean.png', res = 300, width = 4000, height = 2000)
ggplot(data = mean_effects) + 
  geom_point(aes(y = predictor, x = effect, group = effect_type, col = effect_type), 
             position = position_dodge(width = 0.5)) + 
  geom_linerange(aes(y = predictor, xmin = lower_ci, xmax = upper_ci, group = effect_type, col = effect_type), 
                 position = position_dodge(width = 0.5)) + 
  facet_wrap(~response, nrow = 1, scales = 'free_x') + 
  theme_bw() +
  geom_vline(aes(xintercept = 0), lwd = 1)
dev.off()

png('figures/SEM_analysis/effect_tables/effect_plots_all.png', res = 300, width = 5000, height = 2000)
ggplot(data = all_effects) + 
  geom_point(aes(y = predictor, x = effect, group = paste0(effect_type, model_name), col = effect_type, pch = sig), 
             position = position_dodge(width = 0.5)) + 
  #geom_linerange(aes(y = predictor, xmin = lower_ci, xmax = upper_ci, group = paste0(effect_type, model_name), col = effect_type), 
  #               position = position_dodge(width = 0.5)) + 
  facet_wrap(~response, nrow = 1, scales = 'free_x') + 
  theme_bw() +
  geom_vline(aes(xintercept = 0), lwd = 1) + 
  scale_shape_manual(values = c(21, 19))
dev.off()

ggplot(data = all_effects %>% 
         filter(bio_metric == 'latitude_proxy')) + 
  geom_point(aes(y = predictor, x = effect, group = paste0(effect_type, model_name), col = effect_type, pch = sig), 
             position = position_dodge(width = 0.5)) + 
  #geom_linerange(aes(y = predictor, xmin = lower_ci, xmax = upper_ci, group = paste0(effect_type, model_name), col = effect_type), 
  #               position = position_dodge(width = 0.5)) + 
  facet_wrap(~response, nrow = 1, scales = 'free_x') + 
  theme_bw() +
  geom_vline(aes(xintercept = 0), lwd = 1) + 
  scale_shape_manual(values = c(21, 19))


png('figures/SEM_analysis/effect_tables/effect_plots_all_clean.png', res = 300, width = 5000, height = 2000)
ggplot(data = all_effects %>% 
         filter(effect_type != 'total')) + 
  ggbeeswarm::geom_quasirandom(aes(y = predictor, x = effect, fill = effect_type), 
                               dodge.width = 0.5, pch = 21, size = 2) +
  facet_wrap(~response, nrow = 1, scales = 'free_x') + 
  theme_bw() +
  geom_vline(aes(xintercept = 0), lwd = 1) + 
  scale_shape_manual(values = c(21, 19)) + 
  scale_colour_manual(values = c("#E59400", "#8F7CA1"))+ 
  scale_fill_manual(values = c("#E59400", "#8F7CA1"))
dev.off()



png('figures/SEM_analysis/effect_tables/effect_plots_backward_models.png', res = 300, width = 5000, height = 1000)
ggplot(data = all_effects %>% 
         filter(effect_type != 'total', 
                grepl('backward_sem', model_name))) + 
  ggbeeswarm::geom_quasirandom(aes(y = predictor, x = effect, fill = effect_type, pch = sig), 
                               dodge.width = 0.5, size = 2) +
  facet_wrap(~response, nrow = 1, scales = 'free_x') + 
  theme_bw() +
  geom_vline(aes(xintercept = 0), lwd = 1) + 
  scale_shape_manual(values = c(21, 19)) + 
  scale_colour_manual(values = c("#E59400", "#8F7CA1"))+ 
  scale_fill_manual(values = c("#E59400", "#8F7CA1"))
dev.off()

png('figures/SEM_analysis/effect_tables/effect_plots_full_models.png', res = 300, width = 5000, height = 1000)
ggplot(data = all_effects %>% 
         filter(effect_type != 'total', 
                grepl('full_sem', model_name))) + 
  ggbeeswarm::geom_quasirandom(aes(y = predictor, x = effect, col = effect_type, fill = effect_type, pch = sig), 
                               dodge.width = 0.5, size = 2) +
  facet_wrap(~response, nrow = 1, scales = 'free_x') + 
  theme_bw() +
  geom_vline(aes(xintercept = 0), lwd = 1) + 
  scale_shape_manual(values = c(21, 1)) + 
  scale_colour_manual(values = c("#E59400", "#8F7CA1"))+ 
  scale_fill_manual(values = c("#E59400", "#8F7CA1"))
dev.off()




png('figures/SEM_analysis/effect_tables/effect_plots_backward_models_tall.png', 
    res = 500, width = 1500, height = 3000)
ggplot(data = all_effects %>% 
         filter(effect_type != 'total', 
                grepl('backward_sem', model_name))) + 
  ggbeeswarm::geom_quasirandom(aes(y = predictor, x = effect, col = effect_type, fill = effect_type, pch = sig), 
                               dodge.width = 0.5, size = 2) +
  facet_wrap(~response, ncol = 1, scales = 'free_x') + 
  theme_bw() +
  geom_vline(aes(xintercept = 0), lwd = 1) + 
  scale_shape_manual(values = c(21, 19)) + 
  scale_colour_manual(values = c("#E59400", "#8F7CA1"))+ 
  scale_fill_manual(values = c("#E59400", "#8F7CA1")) + 
  theme(aspect.ratio = 0.5, 
        legend.position = 'none', 
        strip.background = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab(NULL)
dev.off()

png('figures/SEM_analysis/effect_tables/effect_plots_full_models_tall.png', res = 600, width = 2000, height = 4000)
ggplot(data = all_effects %>% 
         filter(effect_type != 'total', 
                grepl('full_sem', model_name))) + 
  facet_wrap(~response, nrow = 5, dir = "v", scales = 'free_x') + 
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

