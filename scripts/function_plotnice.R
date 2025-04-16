plot_nice <- function(my_sem, layout = 'tree'){
  
  sem_graph <- plot(my_sem, return = T)
  
  sem_graph$nodes_df <- sem_graph$nodes_df %>% mutate(label = recode(label,
                                                                     'range_richness' = 'species range \n richness',
                                                                     'log_resource_rent' = 'natural \n resource rent',
                                                                     'prehistory_states' = 'pre-colonial \n state', 
                                                                     'governance_strength'  = 'governance \n strength', 
                                                                     'poverty_rate' = 'poverty rate', 
                                                                     'maritime_distance' = 'maritime distance', 
                                                                     'land_distance' = 'land distance'))
  sem_graph$edges_df <- sem_graph$edges_df %>% mutate(color = ifelse(as.numeric(label) > 0, '#005AB5', '#DC3220'))
  
   sem_graph %>%
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
    render_graph(layout=layout)
  
  
}
