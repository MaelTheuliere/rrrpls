#' Importer une ficher rpls csv en Rdata
#'
#' @param file le fichier csv
#' @param annee le millésime du fichier
#' @param geoloc booléen TRUE si on souhaite importer la versin géolocalisée du millésime
#' @param as_sf booléen TRUE si on souhaite convertir le dataframe en spatial dataframe (pour les données géolocalisées)
#'
#' @return la fonction renvoie un dataframe ou un spatial dataframe
#' @export
#' @examples

lire_rpls<-function(file,annee,geoloc=F,as_sf=F){
  if (annee==2017 & geoloc==T) {
    lire_rpls_2017_geoloc(file=file,as_sf=as_sf)
  }
  else {
    if (geoloc==T) {
      stop('pas de possibilité de lire les données géolocalisée pour cette année là')
    }
    if (geoloc==F) {
      stop('pas de possibilité de lire les données non géolocalisée pour cette année là')
    }
  }
}

#' Importer des fichers rpls csv en dataframe
#'
#' @param file le répertoire ou se trouve les fichiers csv
#' @param annee le millésime du fichier
#' @param geoloc booléen TRUE si on souhaite importer la versin géolocalisée du millésime
#' @param as_sf booléen TRUE si on souhaite convertir le dataframe en spatial dataframe (pour les données géolocalisées). Par défaut, utilisation de la projection Lambert93.
#'
#' @return la fonction renvoie un dataframe ou un spatial dataframe qui contient l'ensemble des données contenues dans les csv
#' @export
#' @importFrom purrr map_df
#' @importFrom sf st_as_sf
#' @examples

lire_rep_rpls<-function(dir,annee,geoloc=F,as_sf=F){
  list<-list.files(path=dir)
  list<-paste0(dir,list)
  rpls<-map_df(list,~lire_rpls(file=.x,annee=annee,geoloc=geoloc,as_sf=F))
  if (as_sf==T) {
    rpls<-st_as_sf(rpls,coords=c("x","y"),crs=2154)
    return(rpls)
  }
  else {
    return(rpls)
  }
  return(rpls)
}
