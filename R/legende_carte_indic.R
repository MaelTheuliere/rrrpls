#' Légende pour carte pour rpls
#'
#' @param indicateur indicateur à cartographier
#' @param filtre_zero T si on veut ne pas utiliser les valeurs à zéro pour définir les classes de valeur
#' @param zoom_reg booléen T si on veut la carte régional, F pour la carte national
#' @param variable variable à valoriser : poucentage ou valeur absolue
#' @param na_recode libellé pour les valeur à NA de l'indicateur
#' @return la fonction renvoie un graphique ggplot2
#' @export
#' @importFrom rlang enquo
#' @import magrittr
#' @importFrom dplyr filter
#' @importFrom COGiter cog_df_to_list
#' @importFrom cartography getBreaks
#' @importFrom rlang !!
#' @importFrom dplyr pull
#' @importFrom dplyr mutate
#' @importFrom forcats fct_explicit_na
#' @importFrom forcats fct_relevel
#' @importFrom viridisLite viridis
#' @importFrom dplyr inner_join
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_alpha
#' @importFrom ggplot2 coord_sf
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 theme
#' @importFrom tibble tribble
#' @import patchwork
#' @importFrom patchwork plot_layout
#' @encoding UTF-8

legende_carte_indic<-function(indicateur,
                      filtre_zero=F,
                      zoom_reg=F,
                      variable=Pourcent,
                      na_recode="Pas de logements"){
var=enquo(variable)

  dt<-indicateurs_rpls %>%
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

  dt<-indicateurs_rpls %>%
    cog_df_to_list %>%
    .$epci %>%
    filter(Indicateur==indicateur,
           SousEnsemble=="Ensemble du parc") %>%
    mutate(q=cut(!!var,breaks=bks,labels=round(bks[1:length(bks)-1],1),include.lowest = TRUE)
    )



colors<-viridis(nlevels(dt$q))

if(zoom_reg==T){
dt<-dt %>%
  filter(EPCI %in% (epci %>%
                      filter(str_detect(REGIONS_DE_L_EPCI,params$region_code)) %>%
                      pull(EPCI))
         )
}

legend<-ggplot() +
    geom_histogram(data=dt,aes(x=!!var,fill=q),breaks=bks) +
    scale_color_manual(values=colors)+
    scale_fill_manual(values=colors)+
    theme_minimal()+
    scale_x_continuous(breaks=bks,labels=round(bks,1))+
    labs(x=NULL,y=NULL)+
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.text.y=element_blank(),
          axis.text.x = element_text(hjust=0,size=14)
    )
na<-tribble(~x,~y,
            na_recode,.1) %>%
  ggplot()+
  geom_bar(aes(x=x,weight=y),color="light grey",fill="light grey")+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.y=element_blank(),
        axis.text.x = element_text(size=14)
  )+
  labs(y="",x="")+
  ylim(c(0,2))
legend+na+plot_layout(widths=c(10,1))
}
