
indicateurs_rpls_2018<-TidyRpls::indicateurs_rpls_2018 %>% filter(TypeZone != "Communes")
rpls_par_date_2018 <- TidyRpls::rpls_par_date_2018
use_data(indicateurs_rpls_2018, overwrite = T, internal = F,compress = "xz")
use_data(rpls_par_date_2018, overwrite = T, internal = F,compress = "xz")
