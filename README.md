README
================
2025-02-07

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
library(pacman)
p_load(
  
    # package management
  devtools, 
  
  # data processing
  tidyverse,
  dplyr, 
  tidyr, 
  sf, 
  terra,
  summarytools, 

  # plotting
  tmap,
  GGally, 
  rworldxtra,
  rnaturalearth,
  rnaturalearthdata,
  ggh4x,
  ggrepel, 
  ggsflabel, 
  patchwork, 

  # data analysis
  mclust,
  broom, 
  missForest
  
  )

cat()
sessionInfo()
```

    ## R version 4.3.3 (2024-02-29 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8 
    ## [2] LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## time zone: Europe/Zurich
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] missForest_1.5          broom_1.0.5             mclust_6.1             
    ##  [4] patchwork_1.2.0         ggsflabel_0.0.1         ggrepel_0.9.5          
    ##  [7] ggh4x_0.2.8             rnaturalearthdata_1.0.0 rnaturalearth_1.0.1    
    ## [10] rworldxtra_1.01         sp_2.1-3                GGally_2.2.1           
    ## [13] tmap_3.3-4              summarytools_1.0.1      terra_1.7-71           
    ## [16] sf_1.0-15               lubridate_1.9.3         forcats_1.0.0          
    ## [19] stringr_1.5.1           dplyr_1.1.4             purrr_1.0.2            
    ## [22] readr_2.1.5             tidyr_1.3.1             tibble_3.2.1           
    ## [25] ggplot2_3.5.1           tidyverse_2.0.0         devtools_2.4.5         
    ## [28] usethis_2.2.3           pacman_0.5.1           
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] DBI_1.2.3            tmaptools_3.1-1      tcltk_4.3.3         
    ##  [4] remotes_2.4.2.1      rlang_1.1.3          magrittr_2.0.3      
    ##  [7] matrixStats_1.3.0    e1071_1.7-14         compiler_4.3.3      
    ## [10] png_0.1-8            vctrs_0.6.5          reshape2_1.4.4      
    ## [13] profvis_0.3.8        pkgconfig_2.0.3      fastmap_1.2.0       
    ## [16] backports_1.4.1      magick_2.8.3         ellipsis_0.3.2      
    ## [19] lwgeom_0.2-14        leafem_0.2.3         pander_0.6.5        
    ## [22] utf8_1.2.4           promises_1.2.1       rmarkdown_2.28      
    ## [25] sessioninfo_1.2.2    tzdb_0.4.0           itertools_0.1-3     
    ## [28] xfun_0.47            randomForest_4.7-1.1 cachem_1.1.0        
    ## [31] jsonlite_1.8.8       later_1.3.2          pryr_0.1.6          
    ## [34] parallel_4.3.3       R6_2.5.1             stringi_1.8.3       
    ## [37] RColorBrewer_1.1-3   pkgload_1.4.0        stars_0.6-5         
    ## [40] iterators_1.0.14     Rcpp_1.0.12          knitr_1.48          
    ## [43] base64enc_0.1-3      httpuv_1.6.14        timechange_0.3.0    
    ## [46] tidyselect_1.2.1     rstudioapi_0.16.0    dichromat_2.0-0.1   
    ## [49] abind_1.4-5          yaml_2.3.10          codetools_0.2-19    
    ## [52] miniUI_0.1.1.1       doRNG_1.8.6          pkgbuild_1.4.4      
    ## [55] lattice_0.22-5       leafsync_0.1.0       plyr_1.8.9          
    ## [58] shiny_1.8.1.1        withr_3.0.1          evaluate_0.24.0     
    ## [61] ggstats_0.6.0        units_0.8-5          proxy_0.4-27        
    ## [64] urlchecker_1.0.1     pillar_1.9.0         rngtools_1.5.2      
    ## [67] KernSmooth_2.23-22   foreach_1.5.2        checkmate_2.3.1     
    ## [70] generics_0.1.3       hms_1.1.3            munsell_0.5.1       
    ## [73] scales_1.3.0         xtable_1.8-4         class_7.3-22        
    ## [76] glue_1.7.0           tools_4.3.3          fs_1.6.4            
    ## [79] XML_3.99-0.16.1      rapportools_1.1      grid_4.3.3          
    ## [82] crosstalk_1.2.1      colorspace_2.1-0     raster_3.6-26       
    ## [85] cli_3.6.2            fansi_1.0.6          viridisLite_0.4.2   
    ## [88] gtable_0.3.5         digest_0.6.35        classInt_0.4-10     
    ## [91] htmlwidgets_1.6.4    memoise_2.0.1        htmltools_0.5.8.1   
    ## [94] lifecycle_1.0.4      leaflet_2.2.2        httr_1.4.7          
    ## [97] mime_0.12            MASS_7.3-60.0.1
