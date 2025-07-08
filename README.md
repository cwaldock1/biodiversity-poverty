Systemic bio-inequity links poverty to biodiversity and induces a
conservation paradox
================
2025-07-08

# Abstract

Biodiversity is declining globally while inequity is growing, and
poverty rates are not improving. Global sustainable development
initiatives aim to address biodiversity loss and poverty simultaneously.
Through text analysis of global sustainability policies, we identified a
persistent narrative whereby countries with high biodiversity are
expected to use this natural capital to reduce poverty. Paradoxically,
however, countries with higher biodiversity often experience higher
poverty rates. In a path analysis integrating multiple sources of
biological, historical, and socio-economic data, we show how this
paradox emerges. We find that historical colonial exploitation was
directed by biodiversity gradients, and through socio-economic legacies
continues to influence present-day poverty. Biodiversity increases
poverty rates by having increased historical colonisation probability,
leading to natural resource export-oriented economies with weakened
governance and increased poverty rates. We find no evidence for the
alternative hypothesis that biodiversity directly influences poverty,
either positively or negatively, at a country scale. Whether
sustainability initiatives can jointly address poverty and biodiversity
conservation is challenged by our findings. Moreover, we find that
conservation investments align neither with the global distribution of
biodiversity nor with a country’s economic capacity to protect nature.
As such, conservation initiatives risk reinforcing
biodiversity-associated global inequalities which our work indicates
originated over five centuries ago. The patterns revealed quantitatively
support the proposed “systemic bio-inequity” hypothesis, which argues
that modern biodiversity-poverty linkages are attributed to historical
legacies of ecologically unequal exchange. Our findings challenge the
narrative that natural wealth, and its conservation, will underpin
future economic prosperity of countries. This assumption overlooks
systemic, historical and institutional drivers of modern poverty. A
decolonial approach is needed to address this legacy, whereby
conservationists must not only recognise but challenge and replace
damaging narratives that overlook past and present inequalities in
access to the benefits of biodiversity.

## Info

This repository contains the scripts associated with the manuscript of
Waldock et al. (in prep) “*Systemic bio-inequity links poverty to
biodiversity and induces a conservation paradox”.*

All relevant files to reproduce the work can be found in “scripts”. The
data are available on publication at
<https://doi.org/10.6084/m9.figshare.29457614>.

## Session information

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
```

    ## Installing package into 'C:/Users/cw21p621/AppData/Local/R/win-library/4.4'
    ## (as 'lib' is unspecified)

    ## Warning: unable to access index for repository http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/4.4:
    ##   cannot open URL 'http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/4.4/PACKAGES'

    ## package 'rnaturalearth' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\cw21p621\AppData\Local\Temp\RtmpAZwcAh\downloaded_packages

    ## 
    ## rnaturalearth installed
    ## Installing package into 'C:/Users/cw21p621/AppData/Local/R/win-library/4.4'
    ## (as 'lib' is unspecified)

    ## Warning: unable to access index for repository http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/4.4:
    ##   cannot open URL 'http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/4.4/PACKAGES'

    ## package 'rnaturalearthdata' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\cw21p621\AppData\Local\Temp\RtmpAZwcAh\downloaded_packages

    ## 
    ## rnaturalearthdata installed
    ## Installing package into 'C:/Users/cw21p621/AppData/Local/R/win-library/4.4'
    ## (as 'lib' is unspecified)

    ## Warning: package 'ggsflabel' is not available for this version of R
    ## 
    ## A version of this package for your version of R might be available elsewhere,
    ## see the ideas at
    ## https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages
    ## Warning: unable to access index for repository http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/4.4:
    ##   cannot open URL 'http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/4.4/PACKAGES'

    ## Warning in p_install(package, character.only = TRUE, ...):

    ## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
    ## logical.return = TRUE, : there is no package called 'ggsflabel'

    ## Warning in p_load(devtools, tidyverse, dplyr, tidyr, sf, terra, summarytools, : Failed to install/load:
    ## ggsflabel

``` r
sessionInfo()
```

    ## R version 4.4.3 (2025-02-28 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
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
    ##  [1] missForest_1.5          broom_1.0.8             mclust_6.1.1           
    ##  [4] patchwork_1.3.0         ggrepel_0.9.6           ggh4x_0.3.0            
    ##  [7] rnaturalearthdata_1.0.0 rnaturalearth_1.0.1     rworldxtra_1.01        
    ## [10] sp_2.2-0                GGally_2.2.1            tmap_4.0               
    ## [13] summarytools_1.1.3      terra_1.8-42            sf_1.0-20              
    ## [16] lubridate_1.9.4         forcats_1.0.0           stringr_1.5.1          
    ## [19] dplyr_1.1.4             purrr_1.0.4             readr_2.1.5            
    ## [22] tidyr_1.3.1             tibble_3.2.1            ggplot2_3.5.1          
    ## [25] tidyverse_2.0.0         devtools_2.4.5          usethis_3.1.0          
    ## [28] pacman_0.5.1           
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] RColorBrewer_1.1-3      rstudioapi_0.17.1       jsonlite_2.0.0         
    ##   [4] wk_0.9.4                magrittr_2.0.3          magick_2.8.6           
    ##   [7] farver_2.1.2            rmarkdown_2.29          fs_1.6.5               
    ##  [10] vctrs_0.6.5             memoise_2.0.1           base64enc_0.1-3        
    ##  [13] itertools_0.1-3         htmltools_0.5.8.1       leafsync_0.1.0         
    ##  [16] raster_3.6-32           s2_1.1.7                KernSmooth_2.23-26     
    ##  [19] htmlwidgets_1.6.4       plyr_1.8.9              stars_0.6-8            
    ##  [22] cachem_1.1.0            iterators_1.0.14        mime_0.13              
    ##  [25] lifecycle_1.0.4         pkgconfig_2.0.3         cols4all_0.8           
    ##  [28] R6_2.6.1                fastmap_1.2.0           shiny_1.10.0           
    ##  [31] digest_0.6.37           colorspace_2.1-1        leafem_0.2.3           
    ##  [34] pkgload_1.4.0           crosstalk_1.2.1         lwgeom_0.2-14          
    ##  [37] randomForest_4.7-1.2    spacesXYZ_1.5-1         timechange_0.3.0       
    ##  [40] httr_1.4.7              abind_1.4-8             compiler_4.4.3         
    ##  [43] rngtools_1.5.2          microbenchmark_1.5.0    proxy_0.4-27           
    ##  [46] remotes_2.5.0           withr_3.0.2             pander_0.6.6           
    ##  [49] backports_1.5.0         DBI_1.2.3               logger_0.4.0           
    ##  [52] ggstats_0.9.0           pkgbuild_1.4.7          MASS_7.3-64            
    ##  [55] sessioninfo_1.2.3       tmaptools_3.2           leaflet_2.2.2          
    ##  [58] classInt_0.4-11         tools_4.4.3             units_0.8-7            
    ##  [61] leaflegend_1.2.1        httpuv_1.6.15           glue_1.8.0             
    ##  [64] promises_1.3.2          grid_4.4.3              checkmate_2.3.2        
    ##  [67] reshape2_1.4.4          generics_0.1.3          gtable_0.3.6           
    ##  [70] leaflet.providers_2.0.0 tzdb_0.5.0              class_7.3-23           
    ##  [73] data.table_1.17.0       hms_1.1.3               foreach_1.5.2          
    ##  [76] pillar_1.10.2           later_1.4.1             pryr_0.1.6             
    ##  [79] lattice_0.22-6          tidyselect_1.2.1        miniUI_0.1.1.1         
    ##  [82] knitr_1.50              xfun_0.52               rapportools_1.2        
    ##  [85] matrixStats_1.5.0       stringi_1.8.7           yaml_2.3.10            
    ##  [88] evaluate_1.0.3          codetools_0.2-20        tcltk_4.4.3            
    ##  [91] BiocManager_1.30.25     cli_3.6.4               xtable_1.8-4           
    ##  [94] munsell_0.5.1           dichromat_2.0-0.1       Rcpp_1.0.14            
    ##  [97] png_0.1-8               XML_3.99-0.18           parallel_4.4.3         
    ## [100] ellipsis_0.3.2          doRNG_1.8.6.2           profvis_0.4.0          
    ## [103] urlchecker_1.0.1        viridisLite_0.4.2       scales_1.3.0           
    ## [106] e1071_1.7-16            rlang_1.1.5
