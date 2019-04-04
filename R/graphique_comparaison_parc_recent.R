#' Graphique comparaison par récent pour rpls
#'
#' @param indicateur indicateur à cartographier
#' @param title titre du graphique
#' @param caption bas de page du graphique
#' @param variable variable à valoriser : poucentage ou valeur absolue
#' @param g guide
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
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom hrbrthemes theme_ipsum_rc

graphique_comparaison_parc_recent<-function(indicateur,variable=Pourcent,title="",caption=""){
var<-enquo(variable)
  dfg<-df %>%
    filter(SousEnsemble %in% c("Ensemble du parc","Parc de moins de 5 ans"),
           Indicateur==indicateur,
           (TypeZone %in% c("Départements","Régions","France")|(TypeZone=="Epci" & CodeZone %in% epci_ref$EPCI))) %>%
    select(TypeZone,Zone,SousEnsemble,!!var) %>%
    spread(SousEnsemble,!!var) %>%
    mutate(Zone=fct_drop(Zone) %>% fct_reorder2(TypeZone,Zone,.desc=F))

  p<-ggplot(dfg)+
    geom_segment(aes(x=`Ensemble du parc`,y=Zone,xend=`Parc de moins de 5 ans`,yend=Zone),color=viridis(6)[1],size=2,alpha=.7)+
    geom_point(aes(x=`Ensemble du parc`,y=Zone),fill=viridis(6)[1],color=viridis(6)[1],size=3,stroke=1,shape=21,alpha=1)+
    geom_point(aes(x=`Parc de moins de 5 ans`,y=Zone),fill="white",color=viridis(6)[1],size=3,stroke=1,shape=21)+
    theme_ipsum_rc(grid="XY")+
    xlim(0,NA)+
    annotate("text",
             x=c(dfg[dfg$Zone=="France métropolitaine et DROM",]$`Ensemble du parc`,
                 dfg[dfg$Zone=="France métropolitaine et DROM",]$`Parc de moins de 5 ans`
             ),
             y=c(dfg[dfg$Zone=="France métropolitaine et DROM",]$Zone,
                 dfg[dfg$Zone=="France métropolitaine et DROM",]$Zone
             ),
             label=c("Ensemble du parc","Parc de moins de 5 ans"),
             vjust=1.5,
             size=4.8
    )+
    theme(legend.position = "bottom",
          axis.text.y=element_text(size=18),
          axis.text.x=element_text(size=18),
          plot.caption = element_text(size=16),
          plot.title = element_text(size=22),
          plot.subtitle = element_text(size=18)
    )+
    labs(x="",y="",title=title,caption=caption)
  return(p)
}


