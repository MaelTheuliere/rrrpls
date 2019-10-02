# rrrpls

*rrrpls* est un package pour permettre la production d'un rapport régional sur les données RPLS (Répertoire du Parc Locatif Social).

Il contient un template bookdown paramétrable par région et les fonctions nécessaires à la création des illustrations.

Il nécessite pour l'instant l'utilisation d'un package interne à la sphère de la statistique publique contenant les données elle-même : `TidyRpls`.

A terme, il permettra de se baser sur les données disponibles en opendata.

## Installation

Installation à partir de github :

``` r
remotes::install_github("MaelTheuliere/rrrpls")
remotes::install_gitlab("dreal-datalab/rrrpls")
```

## Usage

- Créez un nouveau projet Rstudio et sélectionner `Publication RPLS` comme type de projet;

- Changez les paramètres ;

- complétez les champs auteurs et date dans index.Rmd ;

- complétez la liste des epci de référence sur lesquels vous voulez un zoom dans les graphiques ;

- Lancez la compilation du bookdown ;

- Intégrez vos analyses ;-)


## Feuille de route

- Charger les données rpls des précédents millésimes

- Créer un template `pagedown`

