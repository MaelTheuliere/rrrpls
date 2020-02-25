
#' Indicateurs précalculés au territoire
#' @name indicateurs_rpls_2019
#' @docType data
#' @title Indicateurs calculés sur RPLS 2019 au territoire (communes, EPCI, Départements, Régions)
#' @format a \code{tbl_df} containing the following fields:
#' \describe{
#' \item{TypeZone}{Communes, Epci, Départements, Régions ou France}
#' \item{Zone}{Libellé du territoire}
#' \item{CodeZone}{Code du territoire}
#' \item{Variable}{Variable à partir de laquelle a été construit l'indicateur}
#' \item{Indicateur}{Libellé de l'indicateur}
#' \item{Valeur}{Valeur de l'indicateur}
#' \item{Pourcent}{Pourcentage de la valeur de l'indicateur par rapport au nombre de logements sociaux total}
#' \item{SousEnsemble}{Ensemble du parc ou parc de moins de 5 ans}
#' \item{Valeur_2018}{Valeur de l'indicateur en 2018}
#' \item{Pourcent}{Pourcentage de la valeur de l'indicateur par rapport au nombre de logements sociaux total en 2018}
#' }
#' @source RPLS 2018.
#' @seealso
#' \link{rrrpls}
NULL

#' Données rpls aggrégés par date et communes
#' @name rpls_par_date_2019
#' @docType data
#' @title Données rpls aggrégés par date et communes
#' @format un \code{tbl_df} contenant les champs suivants:
#' \describe{
#' \item{DEPCOM}{Code commune}
#' \item{construct}{date de construction}
#' \item{dpeenergie}{dpe energie}
#' \item{dpeserre}{dpe GES}
#' \item{nbpiece}{Nombre de piece aggreges a 5 et +}
#' \item{typeconst}{collectif ou individuel}
#' \item{qpv}{en ou hors qpv}
#' \item{n}{nombre de logements}
#' }
#' @source RPLS 2019.
#' @seealso
#' \link{rrrpls}
NULL
