#' Composeur de carte pour rpls
#'
#' @param .data le dataframe de départ
#' @param indicateur indicateur à cartographier
#' @param titre titre du graphique
#' @param soustitre sous-titre du graphique
#' @param basdepage bas de page du graphique
#' @param filtre_zero T si on veut ne pas utiliser les valeurs à zéro pour définir les classes de valeur
#' @param variable variable à valoriser : poucentage ou valeur absolue
#' @param na_recode libellé pour les valeur à NA de l'indicateur
#' @param parc_recent booléen : T si on veut la carte sur le parc de moins de 5 ans
#' @param box vecteur des coordonnées du territore sur lequel zoomer
#' @return la fonction renvoie un graphique ggplot2
#' @export
#' @importFrom rlang enquo
#' @importFrom rlang !!
#' @importFrom rlang .data
#' @import patchwork
#' @encoding UTF-8

composeur_carte <- function(.data=indicateurs_rpls,
                            indicateur,
                            titre = NULL,
                            soustitre = NULL,
                            basdepage = NULL,
                            filtre_zero = F,
                            variable = Pourcent,
                            na_recode = "Pas de logements",
                            parc_recent = T,
                            box = bbox) {
  var <- enquo(variable)
  p1 <- carte_indic(
    .data = .data,
    indicateur = indicateur,
    filtre_zero = filtre_zero,
    zoom_reg = T,
    titre = titre,
    soustitre = soustitre,
    variable = !!var,
    box = bbox
  )
  p2 <- carte_indic(
    .data = .data,
    indicateur = indicateur,
    filtre_zero = filtre_zero,
    zoom_reg = F,
    variable = !!var,
    box = bbox
  )
  if (parc_recent) {
    p3 <- carte_indic(
      .data = .data,
      indicateur = indicateur,
      sousensemble = "Parc de moins de 5 ans",
      filtre_zero = filtre_zero,
      zoom_reg = T,
      soustitre = "Dans le parc récent",
      variable = !!var,
      box = bbox
    )
    p4 <- carte_indic(
      .data = .data,
      indicateur = indicateur,
      sousensemble = "Parc de moins de 5 ans",
      filtre_zero = filtre_zero,
      zoom_reg = F,
      variable = !!var,
      box = bbox
    )
  }
  p5 <- legende_carte_indic(
    .data = .data,
    indicateur = indicateur,
    filtre_zero = filtre_zero,
    na_recode = na_recode,
    variable = !!var
  )


  if (parc_recent) {
    p <- p1 + p2 + p3 + p4 + plot_layout(ncol = 2, widths = c(5, 4))
    return(p / p5 + plot_layout(heights = c(20, 1)))
  }
  else {
    p <- p1 + p2 + plot_layout(ncol = 2, widths = c(5, 4))
    return(p / p5 + plot_layout(heights = c(20, 1)))
  }
}
