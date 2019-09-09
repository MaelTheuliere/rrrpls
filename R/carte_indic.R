#' Carte pour rpls
#'
#' @param .data le dataframe de départ
#' @param indicateur indicateur à cartographier
#' @param zoom_reg booléen T si on veut la carte régional, F pour la carte national
#' @param sousensemble sous ensemble du parc à cartographier
#' @param legend T si on veut une légende
#' @param titre titre du graphique
#' @param soustitre sous-titre du graphique
#' @param basdepage bas de page du graphique
#' @param filtre_zero T si on veut ne pas utiliser les valeurs à zéro pour définir les classes de valeur
#' @param variable variable à valoriser : poucentage ou valeur absolue
#' @param na_recode libellé pour les valeur à NA de l'indicateur
#' @param box vecteur des coordonnées du territore sur lequel zoomer
#' @param g guide
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
#' @importFrom viridisLite viridis
#' @importFrom dplyr inner_join
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_alpha
#' @importFrom ggplot2 coord_sf
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom COGiter cog_df_to_list
#' @encoding UTF-8

carte_indic<-function(.data = indicateurs_rpls,
                      indicateur,
                      zoom_reg=F,
                      sousensemble="Ensemble du parc",
                      legend=F,
                      titre=NULL,
                      soustitre=NULL,
                      basdepage=NULL,
                      filtre_zero=F,
                      variable=Pourcent,
                      na_recode="Pas de logements",
                      box=bbox,
                      g=guide) {
  var=enquo(variable)

  dt<-.data %>%
    filter(Indicateur==indicateur,
           SousEnsemble=="Ensemble du parc") %>%
    cog_df_to_list %>%
    .$epci

  bks<-getBreaks(dt %>% pull(!!var),method="q6") %>%
    unique(.)

  if (filtre_zero) {
    bks<-getBreaks(dt %>%
                     filter(!!var>0) %>%
                     pull(!!var),
                   method="q6") %>%
      unique(.)
  }

  dt<-.data %>%
    cog_df_to_list %>%
    .$epci %>%
    filter(Indicateur==indicateur,
           SousEnsemble==sousensemble) %>%
    mutate(q=cut(!!var,breaks=bks,labels=round(bks[1:length(bks)-1],1),include.lowest = TRUE) %>%
             fct_explicit_na(na_recode) %>%
             fct_relevel(na_recode)
    )



  colors<-dreal_pal("continuous")(nlevels(dt$q))

  if (na_recode %in% levels(dt$q)){
    colors<-c("light grey",dreal_pal("continuous")(nlevels(dt$q)-1))
  }

  p<-  epci_geo %>%
    inner_join(dt
    ) %>%
    ggplot() +
    geom_sf(color="white",size=.1) +
    scale_fill_manual(values=colors) +
    theme_carto()+
    g

  if (zoom_reg==F) {
    p<-p+
      annotate("rect",xmin = box[1],xmax=box[3],ymin = box[2],ymax=box[4],
               fill="white",alpha=0.4)+
      coord_sf(datum=NA) +
      aes(fill=q)+
      labs(fill=NULL)
  }
  else {
    p<-p +
      coord_sf(xlim = c(box[1],box[3]),ylim = c(box[2],box[4]),datum=NA) +
      aes(fill=q,alpha=reg_param) +
      scale_alpha(range=c(.3,1)) +
      labs(title=stringr::str_wrap(titre,50),subtitle=soustitre,fill=NULL,caption=basdepage)
  }
  if (legend==F){
    p<-p +
      theme(legend.position = "none")
  }
  return(p)
}



