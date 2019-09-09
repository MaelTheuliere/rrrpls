#' Importer un ficher rpls 2018 loi csv en Rdata
#'
#' @param file le fichier csv
#' @param passer_au_cog_a_jour booleen TRUE si on souhaite passer les données au millesime 2018 du COG
#'
#' @return la fonction renvoie un dataframe
#'
#' @importFrom readr read_delim
#' @importFrom readr locale
#' @importFrom readr col_character
#' @importFrom readr col_factor
#' @importFrom readr col_date
#' @importFrom readr col_integer
#' @importFrom readr col_number
#' @importFrom readr locale
#' @importFrom dplyr funs
#' @importFrom dplyr mutate_at
#' @importFrom dplyr filter
#' @import magrittr
#' @export
#' @encoding UTF-8

lire_rpls_loi_2018 <- function(file, passer_au_cog_a_jour = TRUE) {
  rpls <- read_delim(
    file = file,
    delim = ";",
    na = c("", "NA", " "),
    locale = locale(
      date_names = "fr", date_format = "%d/%m/%Y",
      time_format = "%AT", decimal_mark = ".",
      grouping_mark = " ", tz = "UTC", encoding = "latin1",
      asciify = FALSE
    ),
    col_types = list(
      age = col_number(),
      AGGLO = col_character(),
      ann_bail = col_integer(),
      bail_red = col_date(format = "%m/%Y"),
      CAT_ORG = col_character(),
      CODSEGPATRIM_red = col_character(),
      COM = col_character(),
      COMABS = col_character(),
      construct_red = col_integer(),
      CONTRESLOG_red = col_character(),
      CONV_red = col_character(),
      CUS_SIGNEE = col_character(),
      DEP = col_character(),
      DEPCOM_red = col_character(),
      DPEDATE_red = col_date(format = "%m/%Y"),
      DPEENERGIE_red = col_character(),
      DPESERRE_red = col_character(),
      DRE = col_character(),
      DROIT_red = col_character(),
      DSU = col_character(),
      duree_vacance = col_integer(),
      EPCI = col_character(),
      finan_cus = col_character(),
      finan_red = col_character(),
      frdom = col_character(),
      IDENT_INT = col_character(),
      IDENT_ORG = col_character(),
      IDENT_REP = col_character(),
      IDENTGES_red = col_character(),
      lib_dep = col_character(),
      lib_reg = col_character(),
      LIBEPCI = col_character(),
      LIBSEGPATRIM_red = col_character(),
      locat_red = col_integer(),
      LOYERMAXAPL_red = col_number(),
      LOYERMAXCUS_red = col_number(),
      loyerprinc_red = col_number(),
      loymoy = col_number(),
      mes_sanscumul = col_character(),
      MISCOMMERCIAL_red = col_character(),
      MODE_red = col_character(),
      modesurf_red = col_character(),
      mois_remlocdate = col_character(),
      nbpiece_red = col_character(),
      NCC = col_character(),
      NRESTH = col_number(),
      OLDLOGT_red = col_character(),
      ORG_ACTIF = col_character(),
      origine_red = col_character(),
      patrimoine_red = col_number(),
      POPCOM2011 = col_number(),
      PRIXVENTE_red = col_number(),
      PRODFIN_red = col_number(),
      QPV_COM = col_character(),
      QPV_red = col_character(),
      QUALACQ_red = col_character(),
      REG = col_character(),
      regdep = col_character(),
      remlocdate_red = col_date(format = "%m/%Y"),
      ROL = col_character(),
      RS = col_character(),
      SIRET = col_character(),
      SORTIEPATRIM_red = col_character(),
      SRU = col_character(),
      surfhab_red = col_number(),
      surfmode_red = col_number(),
      tr_age = col_character(),
      TUR = col_character(),
      TYPECONST_red = col_character(),
      VN = col_character(),
      ZONEPRIX = col_character(),
      ZRR = col_character()
    )
  ) %>%
    purrr::set_names(tolower)


  if (passer_au_cog_a_jour == TRUE) {
    rpls <- rpls %>% passer_au_cog_a_jour(code_commune = depcom_red, aggrege = F, garder_info_supra = F)
  }

  rpls <- mutate_if(rpls, is.character, as.factor)

  return(rpls)
}


#' Importer des fichers rpls csv 2018 en dataframe
#'
#' @param dir le repertoire ou se trouve les fichiers csv
#' @param passer_au_cog_a_jour booleen TRUE si on souhaite passer les données au millesime 2018 du COG
#'
#' @return la fonction renvoie un dataframe qui contient l'ensemble des donnees contenues dans les csv
#' @export
#' @encoding UTF-8

lire_rep_rpls_loi_2018 <- function(dir, passer_au_cog_a_jour = TRUE) {
  list <- list.files(path = dir)
  list <- paste0(dir, list)
  rpls <- furrr::future_map_dfr(list, ~ lire_rpls_loi_2018(file = .x, passer_au_cog_a_jour = passer_au_cog_a_jour))
  rpls <- mutate_if(rpls, is.character, as.factor)
  return(rpls)
}

#' Mettre en forme les donées rpls
#'
#' @param .data le dataframe des données rpls loi 2018
#'
#' @return la fonction renvoie un dataframe qui contient une sélection de données
#' @export
#' @encoding UTF-8

selectionner_rpls_loi_2018 <- function(.data) {
  rpls <- .data %>%
    dplyr::select(ident_rep, DEPCOM, age, construct_red, dpeenergie_red, dpeserre_red, nbpiece_red, typeconst_red, duree_vacance, mode_red, bail_red, locat_red, qpv_red, loyerprinc_red, surfhab_red) %>%
    mutate(
      dpeenergie_red_aggrege = case_when(
        dpeenergie_red %in% c("A", "B", "C") ~ "DPE énergie classe A,B ou C",
        dpeenergie_red %in% c("D") ~ "DPE énergie classe D",
        dpeenergie_red %in% c("E", "F", "G") ~ "DPE énergie classe E,F ou G",
        is.na(dpeenergie_red) ~ "Inconnu",
        TRUE ~ "DPE ENR ERREUR"
      ) %>% as.factor(),
      dpeserre_red_aggrege = case_when(
        dpeserre_red %in% c("A", "B", "C") ~ "DPE GES classe A,B ou C",
        dpeserre_red %in% c("D") ~ "DPE GES classe D",
        dpeserre_red %in% c("E", "F", "G") ~ "DPE GES classe E,F ou G",
        is.na(dpeserre_red) ~ "Inconnu",
        TRUE ~ "DPE GES ERREUR"
      ) %>% as.factor(),
      nbpiece_red_aggrege = case_when(
        nbpiece_red %in% c("1", "2") ~ "moins de 2 pièces",
        nbpiece_red %in% c("3", "4") ~ "3 et 4 pièces",
        TRUE ~ "5 pièces et plus",
      ) %>% as.factor(),
      typeconst_red = fct_recode(typeconst_red,
                                 `Logements collectifs` = "C",
                                 `Logements individuels` = "I",
                                 `Logements Etudiants` = "E"
      ) %>%
        fct_collapse(`Logements collectifs` = c(
          "Logements collectifs",
          "Logements Etudiants"
        )),
      qpv_red=fct_recode(qpv_red,`En QPV`="1",`Hors QPV`="2"),
      DEPCOM = factor(DEPCOM, levels = liste_communes_2019)
    )
}

#' Creation des indicateurs pour la publication
#'
#' @param .data le fichier en entree
#' @param sous_ensemble le sous ensemble du parc pris en compte
#'
#' @return la fonction renvoie un dataframe
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @importFrom dplyr select
#' @importFrom dplyr group_by_if
#' @importFrom dplyr summarise_all
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_if
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr full_join
#' @importFrom tidyr drop_na
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom tidyr complete
#' @importFrom tidyr separate
#' @importFrom COGiter cogifier
#' @export
#' @encoding UTF-8


creer_indicateurs_rpls_2018 <- function(.data, sous_ensemble = "Ensemble du parc") {
  rpls <- .data %>%
    mutate(
      parc_hlm = factor("parc_hlm"),
      n = 1,
      vacance_num = ifelse(duree_vacance > 3 & mode_red %in% c(2), "vacance_num", NA) %>% factor(),
      vacance_denom = ifelse(mode_red %in% c(1, 2), "vacance_denom", NA) %>% factor(),
      mobilite_num = ifelse(year(bail_red) >= 2017 & locat_red < 2017, "mobilite_num", NA) %>% factor(),
      mobilite_denom = ifelse(locat_red < 2017 & mode_red %in% c(1, 2), "mobilite_denom", NA) %>% factor()
    )

  liste_indic1 <- c(
    "parc_hlm", "typeconst_red", "dpeenergie_red_aggrege", "dpeserre_red_aggrege",
    "nbpiece_red_aggrege", "qpv_red", "vacance_num", "vacance_denom",
    "mobilite_num", "mobilite_denom"
  ) %>%
    map(~ rpls %>%
          select(DEPCOM, .x, n) %>%
          group_by_if(is.factor) %>%
          summarise_all(funs(sum)) %>%
          ungroup() %>%
          drop_na() %>%
          spread(.x, n, fill = 0, sep = "_mod_")) %>%
    reduce(full_join, by = "DEPCOM") %>%
    mutate_at(vars(
      vacance_num_mod_vacance_num, vacance_denom_mod_vacance_denom,
      mobilite_num_mod_mobilite_num, mobilite_denom_mod_mobilite_denom
    ), funs(ifelse(is.na(.), 0, .))) %>%
    complete(DEPCOM) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))


  liste_indic2 <- c("loyerprinc_red", "surfhab_red") %>%
    map(~ rpls %>%
          select(DEPCOM, .x) %>%
          group_by(DEPCOM) %>%
          summarise_all(funs(sum)) %>%
          ungroup()) %>%
    reduce(full_join, by = "DEPCOM") %>%
    complete(DEPCOM) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))


  liste_indic <- full_join(liste_indic1, liste_indic2) %>%
    cogifier(metrodrom = T) %>%
    mutate(
      loyer_m2_mod_loyer_m2 = loyerprinc_red / surfhab_red,
      `taux_vacance_mod_Taux de vacance` = 100 * vacance_num_mod_vacance_num / vacance_denom_mod_vacance_denom,
      `taux_mobilite_mod_Taux de mobilité` = 100 * mobilite_num_mod_mobilite_num / mobilite_denom_mod_mobilite_denom
    ) %>%
    mutate_at(
      vars(parc_hlm_mod_parc_hlm:`qpv_red_mod_Hors QPV`),
      funs(pourcent = 100 * . / parc_hlm_mod_parc_hlm)
    ) %>%
    left_join(filocom) %>%
    mutate(parc_hlm_mod_parc_hlm_pourcent = 100 * parc_hlm_mod_parc_hlm / nbrp) %>%
    rename(
      nbrp_mod_nbrp = nbrp,
      loyerprinc_red_mod_loyerprinc_red = loyerprinc_red,
      surfhab_red_mod_surfhab_red = surfhab_red
    )

  liste_indic %<>%
    select(-ends_with("_pourcent"), -ends_with("_denom"), -ends_with("_num")) %>%
    gather(Indicateur, Valeur, -TypeZone, -CodeZone, -Zone) %>%
    full_join(
      liste_indic %>%
        select(TypeZone, CodeZone, Zone, ends_with("_pourcent"), -ends_with("_denom"), -ends_with("_num")) %>%
        gather(Indicateur, Pourcent, -TypeZone, -CodeZone, -Zone) %>%
        mutate(Indicateur = str_replace(Indicateur, "_pourcent", ""))
    ) %>%
    separate(Indicateur, into = c("Variable", "Indicateur"), sep = "_mod_") %>%
    mutate(
      Indicateur = factor(Indicateur),
      Variable = factor(Variable),
      Indicateur = fct_recode(Indicateur,
                              `Loyer au m2` = "loyer_m2",
                              `Loyer total` = "loyerprinc_red",
                              `Nombre de résidences principales` = "nbrp",
                              `Nombre de logements sociaux` = "parc_hlm",
                              `Surface habitable` = "surfhab_red",
      ),
      SousEnsemble = factor(sous_ensemble),
      TypeZone=fct_relevel(TypeZone,"Communes","Epci","Départements","Régions","France"),
      Zone=ifelse(CodeZone=="53147","Mayenne ",as.character(Zone))) %>%
    arrange(TypeZone,desc(Zone)) %>%
    mutate(Zone=fct_inorder(Zone))
  return(liste_indic)
}

#' Creation des indicateurs pour la publication sur les 3 sous ensembles
#'
#' @param .data le fichier en entree
#'
#' @return la fonction renvoie un dataframe
#' @importFrom dplyr bind_rows
#' @export
#' @encoding UTF-8

creer_indicateurs_rpls_2018_publication <- function(.data) {
  data1 <-  creer_indicateurs_rpls_2018(.data,sous_ensemble = "Ensemble du parc")
  data2 <- creer_indicateurs_rpls_2018(.data %>% filter(age<=5),sous_ensemble = "Parc de moins de 5 ans")

  return(data1 %>%
           bind_rows(data2) %>%
           mutate(SousEnsemble=factor(SousEnsemble)) %>%
           arrange(TypeZone,desc(Zone))
  )
}
