#' Graphique comparaison par date de construction pour rpls
#'
#' @param .data le dataframe de départ
#' @param indicateur indicateur à cartographier
#' @param titre titre du graphique
#' @param soustitre sous-titre du graphique
#' @param basdepage bas de page du graphique
#' @param date_debut date de début pour l'analyse
#' @param palette la palette de couleur à utiliser
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
#' @importFrom rlang .data
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

graph_repartition_par_date <- function(.data = rpls_par_date,
                                       indicateur,
                                       titre = NULL,
                                       soustitre = "Par date de construction",
                                       basdepage = NULL,
                                       date_debut = 1950,
                                       palette = "discrete2") {
  var <- enquo(indicateur)
  .data %>%
    filter(construct_red > date_debut) %>%
    select(!!var, construct_red,n) %>%
    group_by(construct_red, !!var) %>%
    summarise(n=sum(n)) %>%
    ungroup() %>%
    ggplot(aes(y = n, x = construct_red, fill = !!var, color = !!var)) +
    geom_bar(stat = "identity") +
    theme_graph() +
    scale_x_continuous(breaks = seq(1950, 2020, 10), expand = c(0, 0)) +
    scale_fill_dreal_d(palette = palette) +
    scale_color_dreal_d(palette = palette) +
    theme(legend.position = "bottom") +
    guides(
      color = guide_legend(
        direction = "horizontal",
        keyheight = unit(2, units = "mm"),
        keywidth = unit(20, units = "mm"),
        order = 1,
        title.position = "right",
        title.hjust = 0.5,
        nrow = 1,
        label.position = "bottom",
        label.hjust = 0
      ),
      fill = guide_legend(
        direction = "horizontal",
        keyheight = unit(2, units = "mm"),
        keywidth = unit(20, units = "mm"),
        order = 1,
        title.position = "right",
        title.hjust = 0.5,
        nrow = 1,
        label.position = "bottom",
        label.hjust = 0
      )
    ) +
    labs(
      title = titre,
      subtitle = soustitre,
      y = "Date de construction",
      x = "",
      fill = "",
      color = "",
      caption = basdepage
    )
}
