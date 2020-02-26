#' Formatage d'un nombre en pourcentage français
#'
#' @param x un nombre
#'
#' @return chaine de caractère
#' @importFrom scales number_format
#' @export
#'
#' @examples
#' nombre_format(10.2)

pourcent_format <- function(x){
  fonction <- number_format(accuracy = .1, decimal.mark = ",", suffix = " %")
  return(fonction(x))
}

#' Formatage d'un nombre en montant en euros
#'
#' @param x un nombre
#'
#' @return chaine de caractère
#' @importFrom scales number_format
#' @export
#'
#' @examples
#' nombre_format(400.5)

euro_m2_format <- function(x){
  fonction <- number_format(accuracy = .1, decimal.mark = ",", suffix = " € / m<sup>2</sup>")
  return(fonction(x))
}


#' Formatage d'un nombre en montant en euros sans le m2 en exposant
#'
#' @param x un nombre
#'
#' @return chaine de caractère
#' @importFrom scales number_format
#' @export
#'
#' @examples
#' nombre_format(400.5)

euro_m2_format_ggplot <- function(x){
  fonction <- number_format(accuracy = .1, decimal.mark = ",", suffix = " € / m2")
  return(fonction(x))
}

#' Formatage d'un nombre avec les conventions françaises
#'
#' @param x un nombre
#'
#' @return chaine de caractère
#' @importFrom scales number_format
#' @export
#'
#' @examples
#' nombre_format(4)

nombre_format <- function(x){
  fonction <- number_format(decimal.mark = ",", big.mark = " ")
  return(fonction(x))
}



#' Carte en rond proportionnel donnant sur une région le taux de logements sociaux et le nombre de logements sociaux
#'
#' @param .data le dataframe de départ
#' @param titre titre du graphique
#' @param soustitre sous-titre du graphique
#' @param basdepage bas de page du graphique
#' @param box vecteur des coordonnées du territore sur lequel zoomer
#' @return la fonction renvoie un graphique ggplot2
#' @export
#' @import magrittr
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom forcats fct_drop
#' @importFrom dplyr inner_join
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 stat_sf_coordinates
#' @importFrom ggplot2 geom_sf_label
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_size_area
#' @importFrom ggplot2 coord_sf
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 scale_size
#' @importFrom COGiter cog_df_to_list
#' @importFrom drealthemes scale_fill_dreal_c
#' @encoding UTF-8

carte_synthese <- function(.data = indicateurs_rpls,
                           titre = NULL,
                           soustitre = NULL,
                           basdepage = NULL,
                           box = bbox) {

  indicateurs <- .data %>%
    filter(Variable == "parc_hlm") %>%
    filter(SousEnsemble == "Ensemble du parc") %>%
    mutate_if(is.factor, fct_drop) %>%
    cog_df_to_list() %>%
    .$epci

  indicateurs_carte_geo <- epci_geo %>%
    inner_join(indicateurs) %>%
    filter(reg_param==1)


  map <- ggplot() +
    #    geom_sf(data = indicateurs_carte_geo, fill = "light grey", color = "white", size = .2) +
    geom_sf(data= epci_geo, aes(alpha = reg_param), fill = "light grey", color = "white", size = .1) +
    geom_sf(data = region, alpha = 0, color = "black", size = .7, linetype = "longdash") +
    stat_sf_coordinates(data = indicateurs_carte_geo, aes(size = Valeur, fill = Pourcent), color = "black", shape = 21, stroke = 1) +
    geom_sf_label(data = indicateurs_carte_geo %>% filter(EPCI %in% epci_ref$EPCI) %>% top_n(6, Valeur), aes(label = NOM_EPCI), nudge_y = 10000, alpha = .5) +
    scale_fill_dreal_c(name = "Taux de \nlogement sociaux") +
    scale_alpha(range = c(.6, 1)) +
    scale_size(name = "Nombre de \nlogements sociaux", range=c(3,20)) +
    coord_sf(xlim = c(box[1], box[3]), ylim = c(box[2], box[4])) +
    theme_void()+
    guides(        colour = F,
                   alpha = F
    )

  return(map)
}


#' chiffre clef - nombre total de logements
#'
#' @param .data le dataframe ou se trouve les indicateurs
#'
#' @return un chaine de charactère
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @export

parc_hlm <- function(.data=indicateurs_rpls){
  .data %>%
    filter(TypeZone=="Régions",
           CodeZone==code_region,
           Indicateur=="Nombre de logements sociaux",
           SousEnsemble=="Ensemble du parc") %>%
    pull(Valeur) %>%
    nombre_format()
}

#' chiffre clef - evolution du parc hlm en pourcent
#'
#' @param .data le dataframe ou se trouve les indicateurs
#'
#' @return un chaine de charactère
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @export

evo_parc_hlm <- function(.data=indicateurs_rpls){
  .data %>%
    filter(TypeZone == "Régions",
           CodeZone == code_region,
           Indicateur == "Nombre de logements sociaux",
           SousEnsemble == "Ensemble du parc") %>%
    mutate(signe = ifelse(Valeur>Valeur_2018, "+", "-"),
           evo_parc_hlm = paste(signe, pourcent_format(100*Valeur/Valeur_2018-100))
    ) %>%
    pull(evo_parc_hlm)
}

#' chiffre clef - pourcentage du logements hlm dans le parc total
#'
#' @param .data le dataframe ou se trouve les indicateurs
#'
#' @return un chaine de charactère
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @export

pourcent_parc_hlm <- function(.data=indicateurs_rpls){
  .data %>%
    filter(TypeZone == "Régions",
           CodeZone == code_region,
           Indicateur == "Nombre de logements sociaux",
           SousEnsemble == "Ensemble du parc") %>%
    pull(Pourcent) %>%
    pourcent_format()
}

#' chiffre clef - pourcentage de logements collectifs dans le parc social
#'
#' @param .data le dataframe ou se trouve les indicateurs
#'
#' @return un chaine de charactère
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @export

pourcent_collectif <- function(.data=indicateurs_rpls){
  .data %>%
    filter(TypeZone == "Régions",
           CodeZone == code_region,
           Indicateur == "Logements collectifs",
           SousEnsemble == "Ensemble du parc") %>%
    pull(Pourcent) %>%
    pourcent_format()
}

#' chiffre clef - pourcentage de logements en QPV dans le parc social
#'
#' @param .data le dataframe ou se trouve les indicateurs
#'
#' @return un chaine de charactère
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @export

pourcent_qpv <- function(.data=indicateurs_rpls){
  .data %>%
    filter(TypeZone == "Régions",
           CodeZone == code_region,
           Indicateur == "En QPV",
           SousEnsemble == "Ensemble du parc") %>%
    pull(Pourcent) %>%
    pourcent_format()
}

#' chiffre clef - loyer au m2 moyen dans le parc social
#'
#' @param .data le dataframe ou se trouve les indicateurs
#'
#' @return un chaine de charactère
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @export

loyer_m2 <- function(.data=indicateurs_rpls){
  .data %>%
    filter(Variable == "loyer_m2",
           TypeZone == "Régions",
           CodeZone == code_region,
           SousEnsemble == "Ensemble du parc") %>%
    mutate(Valeur_texte = euro_m2_format(Valeur)) %>%
    pull(Valeur_texte)
}


#' graphique barre donnant le nombre de logements sociaux sur les principaux epci
#'
#' @param .data le dataframe de départ
#' @param titre titre du graphique
#' @param soustitre sous-titre du graphique
#' @param basdepage bas de page du graphique
#' @param nombre_epci nombre d'epci à afficher
#'
#' @return un ggplot
#' @importFrom dplyr filter
#' @importFrom dplyr mutate_if
#' @importFrom dplyr inner_join
#' @importFrom dplyr top_n
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom COGiter cog_df_to_list
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom drealthemes scale_fill_dreal_d
#' @importFrom drealthemes scale_color_dreal_d
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#' @importFrom forcats fct_inorder
#' @importFrom forcats fct_rev
#' @export

graphique_synthese_parc_epci <- function(.data = indicateurs_rpls,
                                         titre = NULL,
                                         soustitre = NULL,
                                         basdepage = NULL,
                                         nombre_epci = 6){
  indicateurs <- .data %>%
    filter(Variable == "parc_hlm") %>%
    filter(SousEnsemble == "Ensemble du parc") %>%
    mutate_if(is.factor, fct_drop) %>%
    cog_df_to_list() %>%
    .$epci %>%
    inner_join(epci_geo %>% filter(reg_param ==1)) %>%
    top_n(nombre_epci,Valeur) %>%
    arrange(-Valeur) %>%
    mutate(NOM_EPCI = fct_drop(NOM_EPCI) %>% fct_inorder() %>% fct_rev(),
           Valeur_texte = nombre_format(Valeur))

  ggplot(data = indicateurs)+
    geom_bar(aes(x=NOM_EPCI,weight=Valeur,fill=NOM_EPCI)) +
    geom_text(aes(label=Valeur_texte,x=NOM_EPCI,y=Valeur),color="white",family="Roboto",size = 6,hjust=1) +
    geom_text(aes(label=NOM_EPCI,x=NOM_EPCI,y=Valeur,color=NOM_EPCI),family="Roboto",size = 6,hjust=-.1) +
    coord_flip()+
    theme_void()+
    scale_y_continuous(limits=c(0,max(indicateurs$Valeur)*1.4))+
    scale_fill_dreal_d()+
    scale_color_dreal_d()+
    theme(legend.position = "none")+
    labs(x=NULL,y=NULL)

}

#' waffle sur le taux de hlm sur une région
#'
#' @param .data le dataframe de départ
#'
#' @return un ggplot waffle chart
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot
#' @importFrom waffle geom_pictogram
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme_minimal
#' @importFrom waffle theme_enhance_waffle
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 coord_equal
#' @importFrom waffle scale_label_pictogram
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 labs
#' @importFrom drealthemes dreal_cols
#' @importFrom ggplot2 unit
#' @export

graphique_synthese_taux_hlm <- function(.data = indicateurs_rpls) {
  .data %>%
    filter(Variable == "parc_hlm",
           TypeZone == "Régions",
           CodeZone == code_region,
           SousEnsemble == "Ensemble du parc") %>%
    mutate(parc_hlm = Pourcent, parc_non_hlm = 100 - Pourcent) %>%
    select(Variable, parc_hlm, parc_non_hlm) %>%
    pivot_longer(-Variable, names_to = "Indicateur", values_to = "Pourcent") %>%
    ggplot(aes(values = Pourcent, label = Indicateur)) +
    geom_pictogram(aes(colour = Indicateur), n_rows = 5, make_proportional = TRUE) +
    scale_color_manual(values = dreal_cols("primary", "info_light") %>% unname()) +
    theme_minimal()+
    theme_enhance_waffle() +
    theme(plot.margin = unit(c(-10, 0, -10, -.1), "cm")) +
    coord_equal() +
    scale_label_pictogram(
      name = NULL,
      values = c("building", "building")
    ) +
    guides(color = F, label = F) +
    labs(title = NULL)
}

#' waffle sur le taux de collectif dans le parc social
#'
#' @param .data le dataframe de départ
#'
#' @return un ggplot waffle chart
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom waffle geom_pictogram
#' @importFrom drealthemes scale_color_dreal_d
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 unit
#' @importFrom waffle theme_enhance_waffle
#' @importFrom ggplot2 coord_equal
#' @importFrom waffle scale_label_pictogram
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 labs
#'
#' @export

graphique_synthese_collectif <- function(.data = indicateurs_rpls){

  .data %>%
    filter(Variable == "typeconst", TypeZone == "Régions",CodeZone==code_region,SousEnsemble == "Ensemble du parc") %>%
    ggplot(aes(values = Pourcent, label = Indicateur)) +
    geom_pictogram(aes(colour = Indicateur), n_rows = 5, make_proportional = TRUE) +
    scale_color_dreal_d(palette = "discrete2") +
    theme_minimal() +
    theme_enhance_waffle() +
    theme(plot.margin = unit(c(-10, 0, -10, -.1), "cm")) +
    coord_equal() +
    scale_label_pictogram(
      name = NULL,
      values = c("building", "home")
    ) +
    guides(color = F, label = F) +
    labs(title = NULL)

}

#' waffle sur le taux de qpv dans le parc social
#'
#' @param .data le dataframe de départ
#'
#' @return un ggplot waffle chart
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom waffle geom_pictogram
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme_minimal
#' @importFrom waffle theme_enhance_waffle
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 coord_equal
#' @importFrom waffle scale_label_pictogram
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 labs
#' @export


graphique_synthese_taux_qpv <- function(.data = indicateurs_rpls){

  .data %>%
    filter(Variable == "qpv", TypeZone == "Régions",CodeZone==code_region,SousEnsemble == "Ensemble du parc") %>%
    ggplot(aes(values = Pourcent, label = Indicateur)) +
    geom_pictogram(aes(colour = Indicateur), n_rows = 5, make_proportional = TRUE) +
    scale_color_manual(values = c("#008ca0", "#d2d2d2")) +
    theme_minimal() +
    theme_enhance_waffle() +
    theme(plot.margin = unit(c(-12, 0, -12, -.1), "cm")) +
    coord_equal() +
    scale_label_pictogram(
      name = NULL,
      values = c("building", "building")
    ) +
    guides(color = F, label = F) +
    labs(title = NULL)
}

#' graphique barre qui compare le loyer au m2 sur une région sur sur la France entière
#'
#' @param .data le dataframe de départ
#'
#' @return un ggplot graphique barre
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 coord_flip
#' @importFrom drealthemes scale_fill_dreal_d
#' @importFrom drealthemes scale_color_dreal_d
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_y_continuous
#' @export

graphique_synthese_loyer <- function(.data = indicateurs_rpls){
  .data %>%
    filter(Variable == "loyer_m2",
           (TypeZone == "Régions" & CodeZone==code_region)|(CodeZone=="FRMETRODROM"),
           SousEnsemble == "Ensemble du parc") %>%
    mutate(Valeur_texte = euro_m2_format_ggplot(Valeur)) %>%
    ggplot() +
    geom_bar(aes(x = Zone, weight = Valeur, fill = Zone),width = .5) +
    geom_text(aes(x = Zone, y = Valeur / 2, label = Zone), color = "white", size = 10, family = "Roboto") +
    geom_text_repel(aes(x = Zone, y = Valeur, label = Valeur_texte, color = Zone),
                    family = "Roboto", size = 11,
                    hjust = -.2
    ) +
    coord_flip() +
    scale_fill_dreal_d() +
    scale_color_dreal_d() +
    theme_void() +
    theme(legend.position = "none") +
    labs(x = "", y = "") +
    theme(axis.text = element_blank()) +
    scale_y_continuous(limits = c(0, 6.5))

}

