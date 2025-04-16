### read in SEM and make nicer looking figure

pacman::p_load(DiagrammeR, piecewiseSEM)

source('scripts/function_plotnice.R')


### simplified model 
sem <- readRDS("figures/SEM_analysis/model_runs/range_richness/noLat/backward_PR.RDS")

plot_nice(sem, layout = 'tree')

saveWidget(plot_nice(sem), "test.html")
webshot::webshot("test.html", file=file.path("figures/SEM_analysis/plots/noLat_backward_PR.png"), zoom = 3)


#### full model ----

sem <- readRDS("figures/SEM_analysis/model_runs/range_richness/full_model/full_PR.RDS")

plot_nice(sem, layout = 'tree')
saveWidget(plot_nice(sem, layout = 'tree'), "test.html")
webshot::webshot("test.html", file=file.path("figures/SEM_analysis/plots/full_model_PR.png"), zoom = 3)


#### latitude only model ----

sem <- readRDS("figures/SEM_analysis/model_runs/range_richness/latOnly/backward_PR.RDS")

plot_nice(sem, layout = 'tree')
saveWidget(plot_nice(sem, layout = 'tree'), "test.html")
webshot::webshot("test.html", file=file.path("figures/SEM_analysis/plots/latOnly_PR.png"), zoom = 3)
