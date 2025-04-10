#### Create output directory ----

fig_dir <- 'figures/SEM_analysis/model_summaries/plant_richness/'
dir.create(fig_dir, recursive = T)

plot_dir <- 'figures/SEM_analysis/plots/plant_richness/'
dir.create(plot_dir, recursive = T)

model_dir <- 'figures/SEM_analysis/model_runs/plant_richness/'
dir.create(model_dir, recursive = T)

# read in cleaned data
all_data_clean <- readRDS('data/SEM_dataInput.RDS')


full_sem_data_PR <- all_data_clean %>% 
  # select all variables
  select(plant_richness, latitude, log_resource_rent, prehistory_states, colonisation,
         maritime_distance, land_distance, log_resource_rent, governance_strength, 
         debt, poverty_rate, landlocked, agriculture) %>% 
  na.omit %>% 
  data.frame

full_sem_data_NPR <- all_data_clean %>% 
  # select all variables
  select(plant_richness, latitude, log_resource_rent, prehistory_states, colonisation,
         maritime_distance, land_distance, log_resource_rent, governance_strength, 
         debt, national_poverty_headcount, landlocked, agriculture) %>% 
  na.omit %>% 
  data.frame

full_sem_data_MDP <- all_data_clean %>% 
  # select all variables
  select(plant_richness, latitude, log_resource_rent, prehistory_states, colonisation,
         maritime_distance, land_distance, log_resource_rent, governance_strength, 
         debt, MD_poverty, landlocked, agriculture) %>% 
  na.omit %>% 
  data.frame

full_sem_data_GINI <- all_data_clean %>% 
  # select all variables
  select(plant_richness, latitude, log_resource_rent, prehistory_states, colonisation,
         maritime_distance, land_distance, log_resource_rent, governance_strength, 
         debt, gini, landlocked, agriculture) %>% 
  na.omit %>% 
  data.frame


#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################

#### Create set of baseline SEM for POVERTY RATE ----

# specify peicewiseSEM
full_sem_PR <- psem(
  
  # richness predicted by latitude
  lm(plant_richness ~ latitude, data = full_sem_data_PR),
  lm(log_resource_rent ~ latitude, data = full_sem_data_PR),
  lm(prehistory_states ~ plant_richness, data = full_sem_data_PR),
  
  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ prehistory_states + maritime_distance + land_distance + log_resource_rent + plant_richness, family = 'binomial', data = full_sem_data_PR),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_PR),
  
  # debt predicted by colonisation history
  glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_PR),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(poverty_rate ~ debt + governance_strength + landlocked + agriculture + log_resource_rent + plant_richness  + colonisation, 
      family = 'binomial', data = full_sem_data_PR),
  
  data = full_sem_data_PR
  
)

fisherC(full_sem_PR) # model is not valid # p<0.05
summary(full_sem_PR)
write_csv(coefs(full_sem_PR), file.path(fig_dir, 'full_sem_PR.csv'))
saveRDS(full_sem_PR, file.path(model_dir, 'full_sem_PR.RDS'))

saveWidget(plot(full_sem_PR, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "full_sem_PR.png"))


# perform model selection 
backward_sem_PR <- backward_selection(full_sem_PR,  save_path = file.path(fig_dir, 'backward_sem_full_PR.csv'))
saveRDS(backward_sem_PR, file.path(model_dir, 'backward_sem_PR.RDS'))
saveWidget(plot(backward_sem_PR, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "backward_sem_PR.png"))


#### SEM including latitude for pre-colonisal state and colonisation ----

# these are included as were identified as missing d-seperated links
full_sem_withLat_PR <- update(full_sem_PR, prehistory_states ~ range_richness + latitude, colonisation ~ prehistory_states + maritime_distance + land_distance + log_resource_rent + range_richness + latitude)
saveRDS(full_sem_withLat_PR, file.path(model_dir, 'full_sem_withLat_PR.RDS'))

backward_sem_withLat_PR <- backward_selection(full_sem_withLat_PR,  save_path = file.path(fig_dir, 'backward_sem_withLat_PR.csv'))
saveRDS(backward_sem_withLat_PR, file.path(model_dir, 'backward_sem_withLat_PR.RDS'))


#### SEM excluding latitude ----

# specify peicewiseSEM
noLat_sem_PR <- psem(
  
  # richness predicted by latitude
  lm(prehistory_states ~ plant_richness, data = full_sem_data_PR),
  
  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ prehistory_states + maritime_distance + land_distance + log_resource_rent + plant_richness, family = 'binomial', data = full_sem_data_PR),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_PR),
  
  # debt predicted by colonisation history
  glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_PR),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(poverty_rate ~ debt + governance_strength + landlocked + agriculture + log_resource_rent + plant_richness  + colonisation, 
      family = 'binomial', data = full_sem_data_PR),
  
  data = full_sem_data_PR 
  
)

fisherC(noLat_sem_PR) # model is not valid # p<0.05
summary(noLat_sem_PR)
write_csv(coefs(noLat_sem_PR), file.path(fig_dir, 'noLat_sem_PR.csv'))
saveRDS(noLat_sem_PR, file.path(model_dir, 'noLat_sem_PR.RDS'))

saveWidget(plot(noLat_sem_PR, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "noLat_sem_PR.png"))

# perform model selection 
backward_noLat_sem_PR <- backward_selection(noLat_sem_PR,  save_path = file.path(fig_dir, 'backward_sem_noLat_PR.csv'))
saveRDS(backward_noLat_sem_PR, file.path(model_dir, 'backward_noLat_sem_PR.RDS'))

saveWidget(plot(backward_noLat_sem_PR, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "backward_noLat_sem_PR.png"))





#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################


#### Create set of baseline SEM for NATIONAL POVERTY RATE ----

# specify peicewiseSEM
full_sem_NPR <- psem(
  
  # richness predicted by latitude
  lm(plant_richness ~ latitude, data = full_sem_data_NPR),
  lm(log_resource_rent ~ latitude, data = full_sem_data_NPR),
  lm(prehistory_states ~ plant_richness, data = full_sem_data_NPR),
  
  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ prehistory_states + maritime_distance + land_distance + log_resource_rent + plant_richness, family = 'binomial', data = full_sem_data_NPR),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_NPR),
  
  # debt predicted by colonisation history
  glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_NPR),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(national_poverty_headcount ~ debt + governance_strength + landlocked + agriculture + log_resource_rent + plant_richness  + colonisation, 
      family = 'binomial', data = full_sem_data_NPR),
  
  data = full_sem_data_NPR
  
)

fisherC(full_sem_NPR) # model is not valid # p<0.05
summary(full_sem_NPR)
write_csv(coefs(full_sem_NPR), file.path(fig_dir, 'full_sem_NPR.csv'))
saveRDS(full_sem_NPR, file.path(model_dir, 'full_sem_NPR.RDS'))

saveWidget(plot(full_sem_NPR, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "full_sem_NPR.png"))

# perform model selection 
backward_sem_NPR <- backward_selection(full_sem_NPR,  save_path = file.path(fig_dir, 'backward_sem_full_NPR.csv'))
saveRDS(backward_sem_NPR, file.path(model_dir, 'backward_sem_NPR.RDS'))

saveWidget(plot(backward_sem_NPR, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "backward_sem_NPR.png"))



#### SEM including latitude for pre-colonisal state and colonisation ----

# these are included as were identified as missing d-seperated links
full_sem_withLat_NPR <- update(full_sem_NPR, prehistory_states ~ mammal_gd + latitude, colonisation ~ prehistory_states + maritime_distance + land_distance + log_resource_rent + mammal_gd + latitude)
saveRDS(full_sem_withLat_NPR, file.path(model_dir, 'full_sem_withLat_NPR.RDS'))

backward_sem_withLat_NPR <- backward_selection(full_sem_withLat_NPR,  save_path = file.path(fig_dir, 'backward_sem_withLat_NPR.csv'))
saveRDS(backward_sem_withLat_NPR, file.path(model_dir, 'backward_sem_withLat_NPR.RDS'))



#### SEM excluding latitude ----

# specify peicewiseSEM
noLat_sem_NPR <- psem(
  
  # richness predicted by latitude
  lm(prehistory_states ~ plant_richness, data = full_sem_data_NPR),
  
  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ prehistory_states + maritime_distance + land_distance + log_resource_rent + plant_richness, family = 'binomial', data = full_sem_data_NPR),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_NPR),
  
  # debt predicted by colonisation history
  glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_NPR),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(national_poverty_headcount ~ debt + governance_strength + landlocked + agriculture + log_resource_rent + plant_richness  + colonisation, 
      family = 'binomial', data = full_sem_data_NPR),
  
  data = full_sem_data_NPR 
  
)

fisherC(noLat_sem_NPR) # model is not valid # p<0.05
summary(noLat_sem_NPR)
write_csv(coefs(noLat_sem_NPR), file.path(fig_dir, 'noLat_sem_NPR.csv'))
saveRDS(noLat_sem_NPR, file.path(model_dir, 'noLat_sem_NPR.RDS'))

saveWidget(plot(noLat_sem_NPR, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "noLat_sem_NPR.png"))

# perform model selection 
backward_noLat_sem_NPR <- backward_selection(noLat_sem_NPR,  save_path = file.path(fig_dir, 'backward_sem_noLat_NPR.csv'))
saveRDS(backward_noLat_sem_NPR, file.path(model_dir, 'backward_noLat_sem_NPR.RDS'))

saveWidget(plot(backward_noLat_sem_NPR, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "backward_noLat_sem_NPR.png"))



#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################


#### Create set of baseline SEM for MULTIDIMENSIONAL POVERTY ----


# specify peicewiseSEM
full_sem_MDP <- psem(
  
  # richness predicted by latitude
  lm(plant_richness ~ latitude, data = full_sem_data_MDP),
  lm(log_resource_rent ~ latitude, data = full_sem_data_MDP),
  lm(prehistory_states ~ plant_richness, data = full_sem_data_MDP),
  
  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ prehistory_states + maritime_distance + land_distance + log_resource_rent + plant_richness, family = 'binomial', data = full_sem_data_MDP),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_MDP),
  
  # debt predicted by colonisation history
  glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_MDP),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(MD_poverty ~ debt + governance_strength + landlocked + agriculture + log_resource_rent + plant_richness  + colonisation, 
      family = 'binomial', data = full_sem_data_MDP),
  
  data = full_sem_data_MDP
  
)

fisherC(full_sem_MDP) # model is not valid # p<0.05
summary(full_sem_MDP)
write_csv(coefs(full_sem_MDP), file.path(fig_dir, 'full_sem_MDP.csv'))
saveRDS(full_sem_MDP, file.path(model_dir, 'full_sem_MDP.RDS'))

saveWidget(plot(full_sem_MDP, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "full_sem_MDP.png"))

# perform model selection 
backward_sem_MDP <- backward_selection(full_sem_MDP,  save_path = file.path(fig_dir, 'backward_sem_full_MDP.csv'))
saveRDS(backward_sem_MDP, file.path(model_dir, 'backward_sem_MDP.RDS'))

saveWidget(plot(backward_sem_MDP, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "backward_sem_MDP.png"))



#### SEM including latitude for pre-colonisal state and colonisation ----

# these are included as were identified as missing d-seperated links
full_sem_withLat_MDP <- update(full_sem_MDP, prehistory_states ~ mammal_gd + latitude, colonisation ~ prehistory_states + maritime_distance + land_distance + log_resource_rent + mammal_gd + latitude)
saveRDS(full_sem_withLat_MDP, file.path(model_dir, 'full_sem_withLat_MDP.RDS'))

backward_sem_withLat_MDP <- backward_selection(full_sem_withLat_MDP,  save_path = file.path(fig_dir, 'backward_sem_withLat_MDP.csv'))
saveRDS(backward_sem_withLat_MDP, file.path(model_dir, 'backward_sem_withLat_MDP.RDS'))



#### SEM excluding latitude ----

# specify peicewiseSEM
noLat_sem_MDP <- psem(
  
  # richness predicted by latitude
  lm(prehistory_states ~ plant_richness, data = full_sem_data_MDP),
  
  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ prehistory_states + maritime_distance + land_distance + log_resource_rent + plant_richness, family = 'binomial', data = full_sem_data_MDP),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_MDP),
  
  # debt predicted by colonisation history
  glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_MDP),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(MD_poverty ~ debt + governance_strength + landlocked + agriculture + log_resource_rent + plant_richness  + colonisation, 
      family = 'binomial', data = full_sem_data_MDP),
  
  data = full_sem_data_MDP 
  
)

fisherC(noLat_sem_MDP) # model is not valid # p<0.05
summary(noLat_sem_MDP)
write_csv(coefs(noLat_sem_MDP), file.path(fig_dir, 'noLat_sem_MDP.csv'))
saveRDS(noLat_sem_MDP, file.path(model_dir, 'noLat_sem_MDP.RDS'))

saveWidget(plot(noLat_sem_MDP, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "noLat_sem_MDP.png"))

# perform model selection 
backward_noLat_sem_MDP <- backward_selection(noLat_sem_MDP,  save_path = file.path(fig_dir, 'backward_sem_noLat_MDP.csv'))
saveRDS(backward_noLat_sem_MDP, file.path(model_dir, 'backward_noLat_sem_MDP.RDS'))

saveWidget(plot(backward_noLat_sem_MDP, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "backward_noLat_sem_MDP.png"))



#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################


#### Create set of baseline SEM for GINI INEQUALITY ----



# specify peicewiseSEM
full_sem_GINI <- psem(
  
  # richness predicted by latitude
  lm(plant_richness ~ latitude, data = full_sem_data_GINI),
  lm(log_resource_rent ~ latitude, data = full_sem_data_GINI),
  lm(prehistory_states ~ plant_richness, data = full_sem_data_GINI),
  
  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ prehistory_states + maritime_distance + land_distance + log_resource_rent + plant_richness, family = 'binomial', data = full_sem_data_GINI),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_GINI),
  
  # debt predicted by colonisation history
  glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_GINI),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(gini ~ debt + governance_strength + landlocked + agriculture + log_resource_rent + plant_richness + colonisation, 
      family = 'binomial', data = full_sem_data_GINI),
  
  data = full_sem_data_GINI
  
)

fisherC(full_sem_GINI) # model is not valid # p<0.05
summary(full_sem_GINI)
write_csv(coefs(full_sem_GINI), file.path(fig_dir, 'full_sem_GINI.csv'))
saveRDS(full_sem_GINI, file.path(model_dir, 'full_sem_GINI.RDS'))

saveWidget(plot(full_sem_GINI, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "full_sem_GINI.png"))

# perform model selection 
backward_sem_GINI <- backward_selection(full_sem_GINI,  save_path = file.path(fig_dir, 'backward_sem_full_GINI.csv'))
saveRDS(backward_sem_GINI, file.path(model_dir, 'backward_sem_GINI.RDS'))

saveWidget(plot(backward_sem_GINI, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "backward_sem_GINI.png"))


#### SEM including latitude for pre-colonisal state and colonisation ----

# these are included as were identified as missing d-seperated links
full_sem_withLat_GINI <- update(full_sem_GINI, prehistory_states ~ mammal_gd + latitude, colonisation ~ prehistory_states + maritime_distance + land_distance + log_resource_rent + mammal_gd + latitude)
saveRDS(full_sem_withLat_GINI, file.path(model_dir, 'full_sem_withLat_GINI.RDS'))

backward_sem_withLat_GINI <- backward_selection(full_sem_withLat_GINI,  save_path = file.path(fig_dir, 'backward_sem_withLat_GINI.csv'))
saveRDS(backward_sem_withLat_GINI, file.path(model_dir, 'backward_sem_withLat_GINI.RDS'))


#### SEM excluding latitude ----

# specify peicewiseSEM
noLat_sem_GINI <- psem(
  
  # richness predicted by latitude
  lm(prehistory_states ~ plant_richness, data = full_sem_data_GINI),
  
  # colonisation predicted by socio-economic history and biodiversity
  glm(colonisation ~ prehistory_states + maritime_distance + land_distance + log_resource_rent + plant_richness, family = 'binomial', data = full_sem_data_GINI),
  
  lm(governance_strength ~ colonisation + log_resource_rent, data = full_sem_data_GINI),
  
  # debt predicted by colonisation history
  glm(debt ~ colonisation + governance_strength + log_resource_rent, family = 'binomial', data = full_sem_data_GINI),
  
  # proverty predicted by debt, geography, agriculture, and biodiversity
  glm(gini ~ debt + governance_strength + landlocked + agriculture + log_resource_rent + plant_richness  + colonisation, 
      family = 'binomial', data = full_sem_data_GINI),
  
  data = full_sem_data_GINI 
  
)

fisherC(noLat_sem_GINI) # model is not valid # p<0.05
summary(noLat_sem_GINI)
write_csv(coefs(noLat_sem_GINI), file.path(fig_dir, 'noLat_sem_GINI.csv'))
saveRDS(noLat_sem_GINI, file.path(model_dir, 'noLat_sem_GINI.RDS'))

saveWidget(plot(noLat_sem_GINI, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "noLat_sem_GINI.png"))

bootEff(noLat_sem_GINI, R = 10)

# perform model selection 
backward_noLat_sem_GINI <- backward_selection(noLat_sem_GINI,  save_path = file.path(fig_dir, 'backward_sem_noLat_GINI.csv'))
saveRDS(backward_noLat_sem_GINI, file.path(model_dir, 'backward_noLat_sem_GINI.RDS'))

saveWidget(plot(backward_noLat_sem_GINI, layout = 'tree'), "temp.html")
webshot::webshot("temp.html", file=file.path(plot_dir, "backward_noLat_sem_GINI.png"))




#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################


# plant_richness
model_fit_list <- list.files('figures/SEM_analysis/model_runs/plant_richness', full.names = T)

# get 8 of main interest
patterns <- c('PR', 'NPR', 'MDP', 'GINI')
regex <- paste0('(backward|full)_sem_(', paste(patterns, collapse = '|'), ')')
model_subset_list <- model_fit_list[grepl(regex, model_fit_list)]

# read in all models
model_read <- lapply(model_subset_list, readRDS)

# get model effects for the selected models
model_effects <- lapply(seq_along(model_subset_list), function(x){
  
  tryCatch({
    
    # read model
    model_strings <- str_split(model_subset_list[[x]], '/', simplify = T)
    bio_metric <- model_strings[,4]
    split_string <- gsub('.RDS','', model_strings[,5])[[1]]
    split_string <- strsplit(split_string, "_")[[1]]
    pov_metric <- split_string[length(split_string)]
    model_name <- paste(str_split(gsub('.RDS','', model_strings[,5]), '_', simplify = T)[,1:3], collapse = '_')
    
    model1 <- model_read[[x]]
    
    # boostrap model
    boot1 <- bootEff(model1, R = 1000, seed = 1, parallel = 'no')
    boot1_effect <- semEff(boot1, predictor = "plant_richness")
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

effect_dir <- 'figures/SEM_analysis/effect_tables/'
dir.create(effect_dir, recursive = T)

saveRDS(final_results, file.path(effect_dir, 'plant_richness.RDS'))
