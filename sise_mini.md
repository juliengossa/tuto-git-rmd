Tuto Git Rmd
================

## Chargement des données

``` r
sise <- read.csv("sise_mini.csv", header=TRUE)
```

Colonnes :

``` r
colnames(sise)
```

    ##  [1] "RENTREE"              "ETABLISSEMENT"        "Etablissement"       
    ##  [4] "Type.d.établissement" "Discipline"           "Diplôme"             
    ##  [7] "DN_DE"                "Mobilité"             "CURSUS_LMD"          
    ## [10] "NIVEAU"               "Effectifs"            "Niveau"              
    ## [13] "Rentrée"

``` r
sise %>%
  group_by(Rentrée) %>%
  summarise(Ligne = n()) %>%
  ggplot(aes(x=Rentrée,y=Ligne, group=1)) + geom_line()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

![](sise_mini_files/figure-gfm/graphe-1.png)<!-- -->

**Analyse** : Le nombre de lignes dans les données décroit depuis
l’année 2013, ce qui ne nous dit pas grand chose sur les effectifs
étudiants en France.