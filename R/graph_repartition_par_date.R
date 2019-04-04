#' Graphique comparaison par date de construction pour rpls
#'
#' @param indicateur indicateur à cartographier
#' @param titre titre du graphique
#' @param soustitre sous-titre du graphique
#' @param basdepage bas de page du graphique
#' @param date_debut date de début pour l'analyse
#' @return la fonction renvoie un graphique ggplot2
#' @export
#' @importFrom rlang enquo
#' @import magrittr
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom tidyr spread
#' @importFrom dplyr mutate
#' @importFrom forcats fct_drop
#' @importFrom forcats fct_reorder2
#' @importFrom rlang !!
#' @importFrom dplyr pull
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_violin
#' @importFrom ggplot2 scale_color_viridis_d
#' @importFrom ggplot2 scale_fill_viridis_d
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @encoding UTF-8

graph_repartition_par_date<-function(indicateur,
                                     titre=NULL,
                                     soustitre="Par date de construction",
                                     basdepage=NULL,
                                     date_debut=1950){
  var<-enquo(indicateur)
  rpls %>%
    filter(construct_red>date_debut) %>%
    select(!!var,construct_red,age) %>%
    ggplot(aes(x=!!var,y=construct_red,fill=!!var,color=!!var)) +
    geom_violin(scale = "width",adjust = 1)+
    theme_graph() +
    scale_fill_viridis_d(option="D")+
    scale_color_viridis_d(option="D")+
    coord_flip()+
    theme(legend.position = "none")+
    labs(title=titre,
         subtitle=soustitre,
         y="Date de construction",
         x="",
         caption=basdepage)
}
