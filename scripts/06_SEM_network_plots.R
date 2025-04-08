### read in SEM and make nicer looking figure

pacman::p_load(DiagrammeR)


### simplified model 
sem <- readRDS("figures/SEM_analysis/model_runs/range_richness/backward_sem_PR.RDS")

sem_graph <- plot(sem, return = T)

sem_graph$nodes_df <- sem_graph$nodes_df %>% mutate(label = recode(label,
                                                                   'range_richness' = 'species range \n richness',
                                                                   'log_resource_rent' = 'natural \n resource rent',
                                                                   'prehistory_states' = 'pre-colonial \n state', 
                                                                   'governance_strength'  = 'governance \n strength', 
                                                                   'poverty_rate' = 'poverty rate'))
sem_graph$edges_df <- sem_graph$edges_df %>% mutate(color = ifelse(as.numeric(label) > 0, '#005AB5', '#DC3220'))

test_save <- sem_graph %>%
  set_node_attrs(node_attr = "fixedsize",values = FALSE) %>% 
  add_global_graph_attrs(
    attr = "overlap",
    value = "false",
    attr_type = "graph") %>% 
  add_global_graph_attrs(
    attr = "style",
    value = 'empty',
    attr_type = "node") %>% 
  add_global_graph_attrs(
    attr = "fontsize",
    value = 12,
    attr_type = "node") %>% 
  add_global_graph_attrs(
    attr = "penwidth",
    value = 0,
    attr_type = "node") %>% 
  add_global_graph_attrs(
    attr = "arrowsize",
    value = 1.5,
    attr_type = "edge") %>% 
  add_global_graph_attrs(
    attr = "width",
    value = 2,
    attr_type = "edge") %>% 
  render_graph(layout = 'tree')

saveWidget(test_save, "test.html")
webshot::webshot("test.html", file=file.path("figures/SEM_analysis/plots/backward_sem_PR.png"), zoom = 3)



#### full model ----


sem <- readRDS("figures/SEM_analysis/model_runs/range_richness/full_sem_PR.RDS")

sem_graph <- plot(sem, return = T)

sem_graph$nodes_df <- sem_graph$nodes_df %>% mutate(label = recode(label,
                                                                   'range_richness' = 'species range \n richness',
                                                                   'log_resource_rent' = 'natural \n resource rent',
                                                                   'prehistory_states' = 'pre-colonial \n state', 
                                                                   'governance_strength'  = 'governance \n strength', 
                                                                   'poverty_rate' = 'poverty rate',
                                                                   'maritime_distance' = 'maritime \n distance', 
                                                                   'land_distance' = 'land \n distance'))
sem_graph$edges_df <- sem_graph$edges_df %>% mutate(color = ifelse(as.numeric(label) > 0, '#005AB5', '#DC3220'))

test_save <- sem_graph %>%
  set_node_attrs(node_attr = "fixedsize",values = FALSE) %>% 
  add_global_graph_attrs(
    attr = "overlap",
    value = "false",
    attr_type = "graph") %>% 
  add_global_graph_attrs(
    attr = "style",
    value = 'empty',
    attr_type = "node") %>% 
  add_global_graph_attrs(
    attr = "fontsize",
    value = 8,
    attr_type = "node") %>% 
  add_global_graph_attrs(
    attr = "fontsize",
    value = 5,
    attr_type = "edge") %>% 
  add_global_graph_attrs(
    attr = "penwidth",
    value = 0,
    attr_type = "node") %>% 
  add_global_graph_attrs(
    attr = "arrowsize",
    value = 0.5,
    attr_type = "edge") %>% 
  add_global_graph_attrs(
    attr = "width",
    value = 2,
    attr_type = "edge") %>% 
  render_graph(layout = 'kk')
test_save

saveWidget(test_save, "test.html")
webshot::webshot("test.html", file=file.path("figures/SEM_analysis/plots/full_sem_PR.png"), zoom = 3)
