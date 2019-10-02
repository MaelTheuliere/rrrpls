
#' Indicateurs précalculés au territoire
#' @name indicateurs_rpls_2018
#' @docType data
#' @title Indicateurs calculés sur RPLS 2018 au territoire (communes, EPCI, Départements, Régions)
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
#' }
#' @source RPLS 2018.
#' @seealso
#' \link{rrrpls}
NULL

#' Données rpls aggrégés par date et communes
#' @name rpls_par_date_2018
#' @docType data
#' @title Données rpls aggrégés par date et communes
#' @format un \code{tbl_df} contenant les champs suivants:
#' \describe{
#' \item{DEPCOM}{Code commune}
#' \item{construct_red}{date de construction}
#' \item{dpeenergie_red}{dpe energie}
#' \item{dpeserre_red}{dpe GES}
#' \item{nbpiece_red}{Nombre de piece aggreges a 5 et +}
#' \item{typeconst_red}{collectif ou individuel}
#' \item{qpv_red}{en ou hors qpv}
#' \item{n}{nombre de logements}
#' }
#' @source RPLS 2018.
#' @seealso
#' \link{rrrpls}
NULL
