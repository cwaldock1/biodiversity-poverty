library(data.table)
library(mclust)

# developed from https://stackoverflow.com/questions/62389219/simple-way-to-gather-information-from-model-output

clusterVars <- function(dat, col_idx, mod, G, seed=123){
  
  set.seed(123)
  boot <- MclustBootstrap(mod, nboot = 1000, type = "wlbs") #25 for now to speed up
  
  mydat <- data.table(dat, mod$z, cluster = mod$classification)
  
  setnames(mydat, old = paste0("V", seq_len(G)), new=paste0("Prob", seq_len(G)))
  
  a <- mydat[,(mean = lapply(.SD, mean)), by = c("cluster"), 
             .SDcols=colnames(dat)[col_idx]]
  
  lwr <- mydat[,(mean = lapply(.SD, function(x) quantile(x, 0.025))), by = c("cluster"), 
             .SDcols=colnames(dat)[col_idx]]
  
  upr <- mydat[,(mean = lapply(.SD, function(x) quantile(x, 0.975))), by = c("cluster"), 
               .SDcols=colnames(dat)[col_idx]]
  
  a <- setkey(melt(
    a, id.vars = "cluster", variable.name = "Vars", value.name = "mean"), 
    cluster, Vars)
  
  lwr <- setkey(melt(
    lwr, id.vars = "cluster", variable.name = "Vars", value.name = "lwr"), 
    cluster, Vars)
  
  upr <- setkey(melt(
    upr, id.vars = "cluster", variable.name = "Vars", value.name = "upr"), 
    cluster, Vars)
  
  b <- setkey(melt(
    data.table(cluster = seq_len(G), t(summary(boot, what = "se")$mean)), 
    id.vars = "cluster", variable.name = "Vars", value.name = "SE"),
    cluster, Vars)
  
  out <- a[b[lwr[upr]]]
  
  p <- ggplot(out, aes(x=Vars, y=mean, group=factor(cluster))) + 
    geom_line(aes(colour=factor(cluster)))+
    geom_point()+
    geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width = .1)+
    ggtitle("Mean by cluster") +
    expand_limits(y=0) +                        
    #scale_y_continuous(breaks=0:20*4) +         
    labs(x = "Var", y = "Cluster Average")+
    theme_bw() +
    theme()
  
  list(plot = p, table = out, full_data = mydat)  
  
}

# out <- clusterVars(X_1, col_idx = 1:ncol(X_1), G = 5)
