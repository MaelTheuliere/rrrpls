library(tidyverse)
load('../TidyRpls/data/indicateurs_rpls_2019.rda')
load('../TidyRpls/data/rpls_par_date_2019.rda')

indicateurs_rpls_2019<-indicateurs_rpls_2019 %>% filter(TypeZone != "Communes")

use_data(indicateurs_rpls_2019, overwrite = T, internal = F,compress = "xz")
use_data(rpls_par_date_2019, overwrite = T, internal = F,compress = "xz")
