#### Create output directory ----

sum_dir <- 'figures/SEM_analysis/model_summaries/mammal_gd/'
lapply(c('noLat', 'latOnly', 'full_model'), function(x) dir.create(file.path(sum_dir, x), recursive = T))

plot_dir <- 'figures/SEM_analysis/plots/mammal_gd/'
lapply(c('noLat', 'latOnly', 'full_model'), function(x) dir.create(file.path(plot_dir, x), recursive = T))

model_dir <- 'figures/SEM_analysis/model_runs/mammal_gd/'
lapply(c('noLat', 'latOnly', 'full_model'), function(x) dir.create(file.path(model_dir, x), recursive = T))

# read in cleaned data
all_data_clean <- readRDS('data/SEM_dataInput.RDS')


full_sem_data_PR <- all_data_clean %>% 
  # select all variables
  select(mammal_gd, latitude, log_resource_rent, prehistory_states, colonisation,
         maritime_distance, land_distance, log_resource_rent, governance_strength, 
         debt, poverty_rate, landlocked, agriculture, minerals) %>% 
  na.omit %>% 
  data.frame


full_sem_data_NPR <- all_data_clean %>% 
  # select all variables
  select(mammal_gd, latitude, log_resource_rent, prehistory_states, colonisation,
         maritime_distance, land_distance, log_resource_rent, governance_strength, 
         debt, national_poverty_headcount, landlocked, agriculture, minerals) %>% 
  na.omit %>% 
  data.frame


full_sem_data_MDP <- all_data_clean %>% 
  # select all variables
  select(mammal_gd, latitude, log_resource_rent, prehistory_states, colonisation,
         maritime_distance, land_distance, log_resource_rent, governance_strength, 
         debt, MD_poverty, landlocked, agriculture, minerals) %>% 
  na.omit %>% 
  data.frame


full_sem_data_GINI <- all_data_clean %>% 
  # select all variables
  select(mammal_gd, latitude, log_resource_rent, prehistory_states, colonisation,
         maritime_distance, land_distance, log_resource_rent, governance_strength, 
         debt, gini, landlocked, agriculture, minerals) %>% 
  na.omit %>% 
  data.frame



#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################


#### Create set of baseline SEM for POVERTY RATE ----

# specify peicewiseSEM
full_sem_PR <- psem(
  
  # richness predicted by latitude
  mammal_gd        = lm(mammal_gd ~ latitude, data = full_sem_data_PR),
  minerals              = lm(minerals ~ latitude, data = full_sem_data_PR),
  prehistory_states     = glm(prehistory_states ~ mammal_gd , data = full_sem_data_PR, family = 'binomial'),
  
  # colonisation predicted by socio-economic history and biodiversity
  colonisation          = glm(colonisation ~ prehistory_states + maritime_distance + land_distance + 
                                minerals + mammal_gd, family = 'binomial', data = full_sem_data_PR),
  
  log_resource_rent     = glm(log_resource_rent ~ colonisation, data = full_sem_data_PR),
  
  governance_strength   = glm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_PR, family = 'binomial'),
  
  # debt predicted by colonisation history
  debt                  = glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_PR),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  poverty_rate          = glm(poverty_rate ~ debt + governance_strength + landlocked + agriculture +
                                log_resource_rent + mammal_gd + colonisation, 
                              family = 'binomial', data = full_sem_data_PR),
  
  data = full_sem_data_PR
  
  
)

summary(full_sem_PR)
write_csv(coefs(full_sem_PR), file.path(sum_dir, 'full_model', 'full_PR.csv'))
saveRDS(full_sem_PR, file.path(model_dir, 'full_model', 'full_PR.RDS'))

saveWidget(plot_nice(full_sem_PR, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file = file.path(plot_dir, 'full_model', "full_PR.png"))


#### SEM excluding latitude ----

# specify peicewiseSEM
noLat_sem_PR <- psem(
  
  # richness predicted by latitude
  prehistory_states     = glm(prehistory_states ~ mammal_gd, data = full_sem_data_PR, family = 'binomial'),
  
  # colonisation predicted by socio-economic history and biodiversity
  colonisation          = glm(colonisation ~ prehistory_states + maritime_distance + land_distance + 
                                minerals + mammal_gd, family = 'binomial', data = full_sem_data_PR),
  
  log_resource_rent     = glm(log_resource_rent ~ colonisation, data = full_sem_data_PR),
  
  governance_strength   = glm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_PR, family = 'binomial'),
  
  # debt predicted by colonisation history
  debt                  = glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_PR),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  poverty_rate          = glm(poverty_rate ~ debt + governance_strength + landlocked + agriculture +
                                log_resource_rent + mammal_gd + colonisation, 
                              family = 'binomial', data = full_sem_data_PR),
  
  data = full_sem_data_PR  
)

summary(noLat_sem_PR)
write_csv(coefs(noLat_sem_PR), file.path(sum_dir, 'noLat', 'full_PR.csv'))
saveRDS(noLat_sem_PR, file.path(model_dir, 'noLat', 'full_PR.RDS'))

saveWidget(plot_nice(noLat_sem_PR, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir,'noLat', "full_PR.png"))

# perform model selection 
backward_noLat_sem_PR <- backward_selection(noLat_sem_PR,  save_path = file.path(sum_dir, 'noLat', 'backward_PR.csv'))
saveRDS(backward_noLat_sem_PR, file.path(model_dir,'noLat', 'backward_PR.RDS'))

saveWidget(plot_nice(backward_noLat_sem_PR, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir,'noLat', "backward_PR.png"))





#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################


#### Create set of baseline SEM for NATIONAL POVERTY RATE ----

# specify peicewiseSEM
full_sem_NPR <- psem(
  
  # richness predicted by latitude
  mammal_gd        = lm(mammal_gd ~ latitude, data = full_sem_data_NPR),
  minerals              = lm(minerals ~ latitude, data = full_sem_data_NPR),
  prehistory_states     = glm(prehistory_states ~ mammal_gd , data = full_sem_data_NPR, family = 'binomial'),
  
  # colonisation predicted by socio-economic history and biodiversity
  colonisation          = glm(colonisation ~ prehistory_states + maritime_distance + land_distance + 
                                minerals + mammal_gd, family = 'binomial', data = full_sem_data_NPR),
  
  log_resource_rent     = glm(log_resource_rent ~ colonisation, data = full_sem_data_NPR),
  
  governance_strength   = glm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_NPR, family = 'binomial'),
  
  # debt predicted by colonisation history
  debt                  = glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_NPR),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  national_poverty_headcount          = glm(national_poverty_headcount ~ debt + governance_strength + landlocked + agriculture +
                                              log_resource_rent + mammal_gd + colonisation, 
                                            family = 'binomial', data = full_sem_data_NPR),
  
  data = full_sem_data_NPR
  
  
)

summary(full_sem_NPR)
write_csv(coefs(full_sem_NPR), file.path(sum_dir, 'full_model', 'full_NPR.csv'))
saveRDS(full_sem_NPR, file.path(model_dir, 'full_model', 'full_NPR.RDS'))

saveWidget(plot_nice(full_sem_NPR, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file = file.path(plot_dir, 'full_model', "full_NPR.png"))


#### SEM excluding latitude ----

# specify peicewiseSEM
noLat_sem_NPR <- psem(
  
  # richness predicted by latitude
  prehistory_states     = glm(prehistory_states ~ mammal_gd, data = full_sem_data_NPR, family = 'binomial'),
  
  # colonisation predicted by socio-economic history and biodiversity
  colonisation          = glm(colonisation ~ prehistory_states + maritime_distance + land_distance + 
                                minerals + mammal_gd, family = 'binomial', data = full_sem_data_NPR),
  
  log_resource_rent     = glm(log_resource_rent ~ colonisation, data = full_sem_data_NPR),
  
  governance_strength   = glm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_NPR, family = 'binomial'),
  
  # debt predicted by colonisation history
  debt                  = glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_NPR),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  national_poverty_headcount          = glm(national_poverty_headcount ~ debt + governance_strength + landlocked + agriculture +
                                              log_resource_rent + mammal_gd + colonisation, 
                                            family = 'binomial', data = full_sem_data_NPR),
  
  data = full_sem_data_NPR  
)

summary(noLat_sem_NPR)
write_csv(coefs(noLat_sem_NPR), file.path(sum_dir, 'noLat', 'full_NPR.csv'))
saveRDS(noLat_sem_NPR, file.path(model_dir, 'noLat', 'full_NPR.RDS'))

saveWidget(plot_nice(noLat_sem_NPR, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir,'noLat', "full_NPR.png"))

# perform model selection 
backward_noLat_sem_NPR <- backward_selection(noLat_sem_NPR,  save_path = file.path(sum_dir, 'noLat', 'backward_NPR.csv'))
saveRDS(backward_noLat_sem_NPR, file.path(model_dir,'noLat', 'backward_NPR.RDS'))

saveWidget(plot_nice(backward_noLat_sem_NPR, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir,'noLat', "backward_NPR.png"))


#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################


#### Create set of baseline SEM for MULTIDIMENSIONAL POVERTY ----

# specify peicewiseSEM
full_sem_MDP <- psem(
  
  # richness predicted by latitude
  mammal_gd        = lm(mammal_gd ~ latitude, data = full_sem_data_MDP),
  minerals              = lm(minerals ~ latitude, data = full_sem_data_MDP),
  prehistory_states     = glm(prehistory_states ~ mammal_gd , data = full_sem_data_MDP, family = 'binomial'),
  
  # colonisation predicted by socio-economic history and biodiversity
  colonisation          = glm(colonisation ~ prehistory_states + maritime_distance + land_distance + 
                                minerals + mammal_gd, family = 'binomial', data = full_sem_data_MDP),
  
  log_resource_rent     = glm(log_resource_rent ~ colonisation, data = full_sem_data_MDP),
  
  governance_strength   = glm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_MDP, family = 'binomial'),
  
  # debt predicted by colonisation history
  debt                  = glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_MDP),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  MD_poverty          = glm(MD_poverty ~ debt + governance_strength + landlocked + agriculture +
                              log_resource_rent + mammal_gd + colonisation, 
                            family = 'binomial', data = full_sem_data_MDP),
  
  data = full_sem_data_MDP
  
  
)

summary(full_sem_MDP)
write_csv(coefs(full_sem_MDP), file.path(sum_dir, 'full_model', 'full_MDP.csv'))
saveRDS(full_sem_MDP, file.path(model_dir, 'full_model', 'full_MDP.RDS'))

saveWidget(plot_nice(full_sem_MDP, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file = file.path(plot_dir, 'full_model', "full_MDP.png"))


#### SEM excluding latitude ----

# specify peicewiseSEM
noLat_sem_MDP <- psem(
  
  # richness predicted by latitude
  prehistory_states     = glm(prehistory_states ~ mammal_gd, data = full_sem_data_MDP, family = 'binomial'),
  
  # colonisation predicted by socio-economic history and biodiversity
  colonisation          = glm(colonisation ~ prehistory_states + maritime_distance + land_distance + 
                                minerals + mammal_gd, family = 'binomial', data = full_sem_data_MDP),
  
  log_resource_rent     = glm(log_resource_rent ~ colonisation, data = full_sem_data_MDP),
  
  governance_strength   = glm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_MDP, family = 'binomial'),
  
  # debt predicted by colonisation history
  debt                  = glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_MDP),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  MD_poverty          = glm(MD_poverty ~ debt + governance_strength + landlocked + agriculture +
                              log_resource_rent + mammal_gd + colonisation, 
                            family = 'binomial', data = full_sem_data_MDP),
  
  data = full_sem_data_MDP  
)

summary(noLat_sem_MDP)
write_csv(coefs(noLat_sem_MDP), file.path(sum_dir, 'noLat', 'full_MDP.csv'))
saveRDS(noLat_sem_MDP, file.path(model_dir, 'noLat', 'full_MDP.RDS'))

saveWidget(plot_nice(noLat_sem_MDP, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir,'noLat', "full_MDP.png"))

# perform model selection 
backward_noLat_sem_MDP <- backward_selection(noLat_sem_MDP,  save_path = file.path(sum_dir, 'noLat', 'backward_MDP.csv'))
saveRDS(backward_noLat_sem_MDP, file.path(model_dir,'noLat', 'backward_MDP.RDS'))

saveWidget(plot_nice(backward_noLat_sem_MDP, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir,'noLat', "backward_MDP.png"))



#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################


#### Create set of baseline SEM for GINI INEQUALITY ----

# specify peicewiseSEM
full_sem_GINI <- psem(
  
  # richness predicted by latitude
  mammal_gd        = lm(mammal_gd ~ latitude, data = full_sem_data_GINI),
  minerals              = lm(minerals ~ latitude, data = full_sem_data_GINI),
  prehistory_states     = glm(prehistory_states ~ mammal_gd , data = full_sem_data_GINI, family = 'binomial'),
  
  # colonisation predicted by socio-economic history and biodiversity
  colonisation          = glm(colonisation ~ prehistory_states + maritime_distance + land_distance + 
                                minerals + mammal_gd, family = 'binomial', data = full_sem_data_GINI),
  
  log_resource_rent     = glm(log_resource_rent ~ colonisation, data = full_sem_data_GINI),
  
  governance_strength   = glm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_GINI, family = 'binomial'),
  
  # debt predicted by colonisation history
  debt                  = glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_GINI),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  gini          = glm(gini ~ debt + governance_strength + landlocked + agriculture +
                        log_resource_rent + mammal_gd + colonisation, 
                      family = 'binomial', data = full_sem_data_GINI),
  
  data = full_sem_data_GINI
  
  
)

summary(full_sem_GINI)
write_csv(coefs(full_sem_GINI), file.path(sum_dir, 'full_model', 'full_GINI.csv'))
saveRDS(full_sem_GINI, file.path(model_dir, 'full_model', 'full_GINI.RDS'))

saveWidget(plot_nice(full_sem_GINI, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file = file.path(plot_dir, 'full_model', "full_GINI.png"))


#### SEM excluding latitude ----

# specify peicewiseSEM
noLat_sem_GINI <- psem(
  
  # richness predicted by latitude
  prehistory_states     = glm(prehistory_states ~ mammal_gd, data = full_sem_data_GINI, family = 'binomial'),
  
  # colonisation predicted by socio-economic history and biodiversity
  colonisation          = glm(colonisation ~ prehistory_states + maritime_distance + land_distance + 
                                minerals + mammal_gd, family = 'binomial', data = full_sem_data_GINI),
  
  log_resource_rent     = glm(log_resource_rent ~ colonisation, data = full_sem_data_GINI),
  
  governance_strength   = glm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_GINI, family = 'binomial'),
  
  # debt predicted by colonisation history
  debt                  = glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_GINI),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  gini          = glm(gini ~ debt + governance_strength + landlocked + agriculture +
                        log_resource_rent + mammal_gd + colonisation, 
                      family = 'binomial', data = full_sem_data_GINI),
  
  data = full_sem_data_GINI  
)

summary(noLat_sem_GINI)
write_csv(coefs(noLat_sem_GINI), file.path(sum_dir, 'noLat', 'full_GINI.csv'))
saveRDS(noLat_sem_GINI, file.path(model_dir, 'noLat', 'full_GINI.RDS'))

saveWidget(plot_nice(noLat_sem_GINI, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir,'noLat', "full_GINI.png"))

# perform model selection 
backward_noLat_sem_GINI <- backward_selection(noLat_sem_GINI,  save_path = file.path(sum_dir, 'noLat', 'backward_GINI.csv'))
saveRDS(backward_noLat_sem_GINI, file.path(model_dir,'noLat', 'backward_GINI.RDS'))

saveWidget(plot_nice(backward_noLat_sem_GINI, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir,'noLat', "backward_GINI.png"))


#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################

# range richness
model_fit_list <- list.files('figures/SEM_analysis/model_runs/mammal_gd', full.names = T, recursive = T)

# get 8 of main interest
# patterns <- c('PR', 'NPR', 'MDP', 'GINI')
# regex <- paste0('(backward|full)_sem_(', paste(patterns, collapse = '|'), ')')
# model_subset_list <- model_fit_list[grepl(regex, model_fit_list)]

# read in all models
model_read <- lapply(model_fit_list, readRDS)

# get model effects for the selected models
model_effects <- lapply(seq_along(model_fit_list), function(x){
  
  tryCatch({
    # read model
    model_strings <- str_split(model_fit_list[x], '/', simplify = T)
    bio_metric <- model_strings[,4]
    split_string <- gsub('.RDS','', model_strings[,6])[[1]]
    split_string <- strsplit(split_string, "_")[[1]]
    pov_metric <- split_string[length(split_string)]
    model_name <- gsub('.RDS', '', paste(model_strings[,4:6], collapse = '_'))
    
    model1 <- model_read[[x]]
    
    # boostrap model
    boot1 <- bootEff(model1, R = 10000, seed = 1, parallel = 'no')
    boot1_effect <- semEff(boot1, predictor = "mammal_gd")
    eff_table <- getEffTable(boot1_effect) %>% 
      filter(effect_type != 'mediators') %>% 
      mutate(model_name = model_name, 
             pov_metric = pov_metric, 
             bio_metric = bio_metric)
    
    return(eff_table)
  }, error = function(e) {
    message(sprintf("Error on model index %d: %s", x, e$message))
    return(NULL)
  })
  
})

final_results <- bind_rows(model_effects[!sapply(model_effects, is.null)])

saveRDS(final_results, file.path(effect_dir, 'mammal_gd.RDS'))
