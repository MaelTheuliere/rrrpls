#' Carte du taux d'évolution
#'
#' @param .data le dataframe en entrée
#' @param zoom_reg booléen T si on veut la carte régional, F pour la carte national
#' @param na_recode chaine de caractères, le libellé qui s'affichera sur la carte pour le valeurs manquantes
#' @param box vecteur des coordonnées du territoire sur lequel zoomer
#'
#' @return la fonction renvoie un graphique ggplot2
#' @export
#' @importFrom rlang enquo
#' @import magrittr
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom cartography getBreaks
#' @importFrom rlang !!
#' @importFrom rlang .data
#' @importFrom dplyr pull
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom forcats fct_explicit_na
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_inorder
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

carte_taux_evolution <- function(.data = indicateurs_rpls,
                                  zoom_reg = F,
                                  na_recode = "Pas de parc social",
                                  box = bbox) {
  dt <- .data %>%
    filter(
      Indicateur == "Nombre de logements sociaux",
      SousEnsemble %in% c("Ensemble du parc")
    ) %>%
    select(TypeZone, CodeZone, Zone, Indicateur, Valeur, Valeur_2018) %>%
    mutate(evolution = Valeur * 100 / Valeur_2018 - 100) %>%
    arrange(evolution) %>%
    mutate(
      q = case_when(
        evolution < -0.5 ~ "< -0,5 %",
        evolution < 0 ~ "de -0,5 à 0 %",
        evolution <= .5 ~ "de 0 à 0,5 %",
        evolution <= 2.5 ~ "de 0,5 à 2.5 %",
        evolution > 2.5 ~ "> 2,5 %"
      ) %>% fct_inorder() %>%
        fct_explicit_na(na_recode) %>%
        fct_relevel(na_recode)
    ) %>%
    cog_df_to_list() %>%
    .$epci %>%
    select(EPCI, NOM_EPCI, evolution, q)



  colors <- dreal_pal("continuous")(nlevels(dt$q))

  if (na_recode %in% levels(dt$q)) {
    colors <- c("light grey", dreal_pal("continuous")(nlevels(dt$q) - 1))
  }


  data_map <- epci_geo %>%
    inner_join(dt)
  if (zoom_reg == F) {
    map <- data_map %>%
      ggplot() +
      annotation_map_tile(zoom = 7, cachedir = "data", type = "cartolight") +
      geom_sf(color = "white", size = .1, aes(fill = q)) +
      geom_sf(data = region, alpha = 0, color = "black", size = .3, linetype = "longdash") +
      scale_fill_manual(values = colors) +
      theme_carto() +
      guides(
        colour = F,
        alpha = F,
        order = 0,
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
      annotate("rect",
               xmin = box[1], xmax = box[3], ymin = box[2], ymax = box[4],
               fill = "white", alpha = 0.4
      ) +
      coord_sf(datum = NA) +
      theme(legend.position = "none")
  }

  else {
    map <- data_map %>%
      ggplot() +
      annotation_map_tile(zoom = 7, cachedir = "data", type = "cartolight") +
      geom_sf(color = "white", size = .1, aes(fill = q, alpha = reg_param)) +
      geom_sf(data = region, alpha = 0, color = "black", size = .3, linetype = "longdash") +
      scale_fill_manual(values = colors) +
      theme_carto() +
      guides(
        colour = F,
        alpha = F,
        order = 0,
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
      coord_sf(xlim = c(box[1], box[3]), ylim = c(box[2], box[4]), datum = NA) +
      scale_alpha(range = c(.3, 1)) +
      labs(title = str_wrap(glue("Croissance du parc social entre {annee_precedente} et {params$annee}"), 50), subtitle = "En %", fill = "")
  }
  return(map)
}
