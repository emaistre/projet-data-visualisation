# 🌍 Projet de Visualisation des Données Erasmus

Ce projet présente une application interactive de visualisation des données relatives aux échanges Erasmus, en utilisant **Shiny** pour créer des visualisations interactives en R. L'application permet de visualiser des données sur les étudiants entrants et sortants d'Erasmus, en fonction des pays, des villes et des universités.

Trois visualisations principales ont été réalisées :

- **Treemap** : Visualisation du nombre d'étudiants entrants et sortants par pays, ville, université.
- **Chord Diagram** : Visualisation des échanges entre pays, permettant de choisir et comparer les flux d'étudiants.
- **Choropleth Map** : Carte interactive représentant les flux d'étudiants entrants et sortants selon les pays européens.

---

## 📁 Structure du projet

- `treemap/`
  - `data/` : Données nécessaires à la visualisation du treemap
  - `app.R` : Application Shiny pour la visualisation du Treemap

- `chord_diagram/`
  - `data/` : Données nécessaires au Chord Diagram
  - `app.R` : Application Shiny pour la visualisation du Chord Diagram

- `choropleth_map/`
  - `data/` : Données nécessaires à la carte choroplèthe
  - `app.R` : Application Shiny pour la visualisation de la Choropleth Map

- `rapport.pdf` : Rapport détaillant les différentes visualisations
- `README.md` : Fichier de présentation du projet (celui-ci)

---

## 🚀 Lancer l'application

Pour chaque visualisation, le fichier `app.R` dans chaque sous-dossier permet de lancer l'application Shiny correspondante.

### Prérequis

- R version 4.0 ou supérieure
- Les packages suivants : `shiny`, `tidyverse`, `chorddiag`, `leaflet`, `dplyr`, `rnaturalearth`, `plotly`
  
Tu peux installer ces packages avec la commande suivante dans R :
```r
install.packages(c("shiny", "treemap", "chorddiag", "leaflet", "dplyr", "rnaturalearth", "plotly"))
```

## 📈 Rapport

Le fichier **`report.pdf`** contient un rapport détaillé sur les visualisations, expliquant les méthodes de création des graphiques, l'interprétation des données, et les conclusions tirées des résultats.

## Visualisations

Treemap: https://j4ohnm-tobias-engelbrecht.shinyapps.io/Treemap/   
Chord diagram: https://j4ohnm-tobias-engelbrecht.shinyapps.io/ChordDiagram/  
 


