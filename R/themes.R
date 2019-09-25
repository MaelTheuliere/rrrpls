#' Theme ggplot pour rpls
#'
#' @return la fonction renvoie un theme
#' @export
#' @importFrom drealthemes theme_dreal
#' @importFrom ggplot2 theme
#' @encoding UTF-8

theme_carto<-function(){
  theme_dreal()+
    theme(plot.title = element_text(size=24),
          plot.subtitle = element_text(size=22),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          line=element_blank(),
          rect =element_blank(),
          legend.position = "top",
          panel.background=element_rect(fill="#ffffff",color="#ffffff"),
          plot.background=element_rect(fill="#ffffff",color="#ffffff")
    )
}

#' Theme ggplot pour rpls
#'
#' @return la fonction renvoie un theme
#' @export
#' @importFrom drealthemes theme_dreal
#' @importFrom ggplot2 theme

theme_graph <-function(){
  theme_dreal()+
  theme(plot.title = element_text(size=24),
        plot.subtitle = element_text(size=22),
        axis.text.y=element_text(size=18),
        axis.text.x=element_text(size=18),
        plot.caption = element_text(size=18)
  )
}
