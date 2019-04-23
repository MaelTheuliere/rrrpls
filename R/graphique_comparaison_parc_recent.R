#' Graphique comparaison par récent pour rpls
#'
#' @param indicateur indicateur à cartographier
#' @param title titre du graphique
#' @param caption bas de page du graphique
#' @param variable variable à valoriser : poucentage ou valeur absolue
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

graphique_comparaison_parc_recent<-function(indicateur,
                                            variable=Pourcent,
                                            title="",
                                            caption=""){
var<-enquo(variable)
  dfg_long <- df %>%
    filter(SousEnsemble %in% c("Ensemble du parc","Parc de moins de 5 ans"),
           Indicateur==indicateur,
           (TypeZone %in% c("Départements","Régions","France")|(TypeZone=="Epci" & CodeZone %in% epci_ref$EPCI))) %>%
    select(TypeZone,Zone,SousEnsemble,!!var)

dfg_large <- dfg_long %>%
    spread(SousEnsemble,!!var) %>%
    mutate(Zone=fct_drop(Zone) %>% fct_reorder2(TypeZone,Zone,.desc=F))

max<-max(dfg_long %>% pull(!!var))*.75
min<-max(dfg_long %>% pull(!!var))*.25
test_min_max<-dfg_large[dfg_large$Zone=="Pays de la Loire",]$`Ensemble du parc`>dfg_large[dfg_large$Zone=="Pays de la Loire",]$`Parc de moins de 5 ans`

if (test_min_max) {
dt<-tribble(
  ~`Parc de moins de 5 ans`,~`Ensemble du parc`,
  min,max
)}
else {
  dt<-tribble(
    ~`Parc de moins de 5 ans`,~`Ensemble du parc`,
    max,min
  )
}

l<-ggplot(dt)+
  geom_segment(aes(x=`Ensemble du parc`,
                   y=1,
                   xend=`Parc de moins de 5 ans`,
                   yend=1),
               color="grey",size=2,alpha=.7)+
  geom_point(aes(x=`Ensemble du parc`,y=1),fill=viridis(6)[1],color=viridis(6)[1],size=3,stroke=1,shape=21,alpha=1)+
  geom_point(aes(x=`Parc de moins de 5 ans`,y=1),fill="white",color=viridis(6)[1],size=3,stroke=1,shape=21)+
  theme_void()+
  theme(plot.caption = element_text(size=16))+
  annotate("text",
           x=c(min,
               max
           ),
           y=c(1,
               1
           ),
           label=c("Ensemble du parc","Parc de moins de 5 ans"),
           vjust=1.5,
           size=4.8
  )+
  xlim(0,max*4/3)+
  labs(caption=caption)

p<-ggplot(dfg_large)+
  geom_segment(aes(x=`Ensemble du parc`,
                   y=Zone,
                   xend=`Parc de moins de 5 ans`,
                   yend=Zone),
               color="grey",size=2,alpha=.7)+
  geom_point(aes(x=`Ensemble du parc`,y=Zone),fill=viridis(6)[1],color=viridis(6)[1],size=3,stroke=1,shape=21,alpha=1)+
  geom_point(aes(x=`Parc de moins de 5 ans`,y=Zone),fill="white",color=viridis(6)[1],size=3,stroke=1,shape=21)+
  hrbrthemes::theme_ipsum_rc(grid="XY")+
  xlim(0,NA)+
  theme(legend.position = "bottom",
        axis.text.y=element_text(size=18),
        axis.text.x=element_text(size=18),
        plot.caption = element_text(size=16),
        plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18)
  )+
  labs(x="",y="",title=title)

res<-p+l+plot_layout(ncol=1,heights=c(8,1))
return(res)
}


