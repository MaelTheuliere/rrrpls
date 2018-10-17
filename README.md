# rrrpls

rrrpls est un package pour permettre la lecture des données RPLS (Répertoire du Parc Locatif Social).
Il permet de lire les données disponibles en opendata ou celles disponibles au sein de la sphère statistique publique.

## Installation

Installation à partir de github :

``` r
devtools::install_github("MaëlTheuliere/rrrpls")
```

## Exemple

Pour l'instant, les seules données lisibles sont celles de 2017 géolocalisées.

``` r
lire_rpls("geoloc2017_reg01.csv",annee=2017,geoloc=T,as_sf=T)
```
## Feuille de route

- Permettre le chargement des données rpls opendata

- Charger les données rpls des autres millésimes

- Créer des sorties de valorisations standards sur les données

