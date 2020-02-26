#' Carte sur le poids du parc récent
#'
#' @param .data le dataframe en entrée
#' @param zoom_reg booléen T si on veut la carte régional, F pour la carte national
#' @param na_recode chaine de caractères, le libellé qui s'affichera sur la carte pour le valeurs manquantes
#' @param box vecteur des coordonnées du territoire sur lequel zoomer
#' @return la fonction renvoie un graphique ggplot2
#' @export
#' @importFrom rlang enquo
#' @import magrittr
#' @importFrom dplyr filter
#' @importFrom cartography getBreaks
#' @importFrom rlang !!
#' @importFrom rlang .data
#' @importFrom dplyr pull
#' @importFrom dplyr mutate
#' @importFrom forcats fct_explicit_na
#' @importFrom forcats fct_relevel
#' @importFrom drealthemes scale_fill_dreal_d
#' @importFrom drealthemes dreal_pal
#' @importFrom dplyr inner_join
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_alpha
#' @importFrom ggplot2 coord_sf
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 guide_legend
#' @importFrom stringr str_wrap
#' @importFrom ggspatial annotation_map_tile
#' @importFrom glue glue
#' @importFrom COGiter cog_df_to_list
#' @encoding UTF-8

carte_parc_recent<-function(.data=indicateurs_rpls,
                                 zoom_reg=F,
                                 na_recode="Pas de parc récent",
                                 box=bbox) {
  dt<-.data %>%
    filter(Indicateur=="Nombre de logements sociaux",
           SousEnsemble %in% c("Ensemble du parc","Parc de moins de 5 ans")) %>%
    select(TypeZone,CodeZone,Zone,Indicateur,Valeur,SousEnsemble) %>%
    spread(SousEnsemble,Valeur) %>%
    mutate(part=100*`Parc de moins de 5 ans`/`Ensemble du parc`) %>%
    cog_df_to_list %>%
    .$epci %>%
    select(EPCI,NOM_EPCI,part)


  bks<-getBreaks(dt %>% filter(part>0) %>% pull(part),method="q6")

  dt<-dt %>%
    mutate(q=cut(part,breaks=bks,labels=round(bks[1:length(bks)-1],1),include.lowest = TRUE) %>%
             fct_explicit_na(na_recode) %>%
             fct_relevel(na_recode))

  colors<-dreal_pal("continuous")(nlevels(dt$q))

  if (na_recode %in% levels(dt$q)){
    colors<-c("light grey",dreal_pal("continuous")(nlevels(dt$q)-1))
  }


  data_map <-  epci_geo %>%
    inner_join(dt
    )
  if (zoom_reg == F) {

  map <- data_map %>%
    ggplot() +
    annotation_map_tile(zoom = 7, cachedir = "data", type = "cartolight") +
    geom_sf(color="white",size=.1,aes(fill=q)) +
    geom_sf(data = region, alpha = 0, color = "black", size = .3, linetype = "longdash") +
    scale_fill_manual(values=colors) +
    theme_carto()+
    guides(colour=F,
           alpha=F,
           order=0,
           fill=guide_legend(direction="horizontal",
                             keyheight=unit(2,units="mm"),
                             keywidth=unit(20,units="mm"),
                             order=1,
                             title.position="right",
                             title.hjust=0.5,
                             nrow=1,
                             label.position="bottom",
                             label.hjust=0))+
    annotate("rect",xmin = box[1],xmax=box[3],ymin = box[2],ymax=box[4],
                                                            fill="white",alpha=0.4)+
    coord_sf(datum=NA) +
    theme(legend.position = "none")

}

  else {
    map <- data_map %>%
      ggplot() +
      annotation_map_tile(zoom = 7, cachedir = "data", type = "cartolight") +
      geom_sf(color="white",size=.1,aes(fill=q,alpha=reg_param)) +
      geom_sf(data = region, alpha = 0, color = "black", size = .3, linetype = "longdash") +
      scale_fill_manual(values=colors) +
      theme_carto()+
      guides(colour=F,
             alpha=F,
             order=0,
             fill=guide_legend(direction="horizontal",
                               keyheight=unit(2,units="mm"),
                               keywidth=unit(20,units="mm"),
                               order=1,
                               title.position="right",
                               title.hjust=0.5,
                               nrow=1,
                               label.position="bottom",
                               label.hjust=0))+
      coord_sf(xlim = c(box[1],box[3]),ylim = c(box[2],box[4]),datum=NA) +
      scale_alpha(range=c(.3,1)) +
      labs(title=str_wrap(glue("Poids du parc récent dans l'ensemble du parc social au 1er janvier {params$annee}"),50),subtitle="En %",fill="")
  }
  return(map)
}
