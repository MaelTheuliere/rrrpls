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

- Charger le package `TidyRpls` avec les données.

- Créez un nouveau projet Rstudio et sélectionner `Publication RPLS` comme type de projet.

- Changez les paramètres

- Lancez la compilation du bookdown

- Intégrez vos analyses ;-)


## Feuille de route

- Permettre le chargement des données rpls opendata pour ouvrir le 

- Charger les données rpls des précédents millésimes

- Créer un template `pagedown`

