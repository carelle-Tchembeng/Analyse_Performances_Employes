# Analyse des Performances Employés 📈

> Exploration statistique et détection d'anomalies sur les données RH d'une entreprise — R

[![R](https://img.shields.io/badge/R-4.x-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![dplyr](https://img.shields.io/badge/dplyr-Data_Manipulation-blue)](https://dplyr.tidyverse.org/)
[![ggplot2](https://img.shields.io/badge/ggplot2-Visualisation-red)](https://ggplot2.tidyverse.org/)

---

## Contexte métier

Une entreprise souhaite mieux comprendre la répartition des performances de ses employés : identifier les écarts significatifs, détecter les cas atypiques (outliers), et dégager des pistes d'amélioration concrètes à partir des données RH.

---

## Objectifs

- Étudier la **distribution des scores de performance** et des **heures travaillées**
- Calculer les indicateurs statistiques clés pour caractériser la population
- Détecter les employés outliers à l'aide de la **règle des 1.5 × IQR**
- Produire des visualisations claires et exploitables par les équipes RH
- Synthétiser les résultats dans un rapport structuré

---

## Compétences démontrées

### Statistiques descriptives
| Indicateur | Description |
|---|---|
| Moyenne, médiane, mode | Mesures de tendance centrale |
| Variance, écart-type | Mesures de dispersion |
| Quartiles (Q1, Q3) | Répartition de la distribution |
| IQR (Interquartile Range) | Base de détection des outliers |

### Visualisation des données
- **Histogrammes** — distribution des scores et des heures travaillées
- **Boxplots** — visualisation des quartiles et des outliers
- **Pie charts** — répartition par catégorie de performance
- **Diagrammes en barres** — comparaisons inter-groupes

### Détection des outliers
Méthode statistique basée sur la règle des **1.5 × IQR** :
```
Borne inférieure = Q1 − 1.5 × IQR
Borne supérieure = Q3 + 1.5 × IQR
Tout point hors de ces bornes = outlier
```

### Manipulation des données
- Nettoyage et transformation avec **dplyr** (tidyverse)
- Filtrage, regroupement et agrégation des données RH

---

## Architecture du projet

```
ANALYSE_PERFORMANCES_EMPLOYES/
├── DataPerformances.csv           ← Données RH brutes
├── projet_performance.R           ← Script principal (point d'entrée)
├── Manipulation_Donnees.R         ← Nettoyage et transformation (dplyr)
├── Statistiques_Descriptives.R    ← Calcul des indicateurs statistiques
├── Detection_Outliers.R           ← Détection via règle 1.5 × IQR
├── Visualisation.R                ← Graphiques (histogrammes, boxplots...)
├── Synthese.R                     ← Résumé consolidé des résultats
├── rapport_performances_R.docx    ← Rapport final avec analyses et conclusions
├── .RData                         ← Environnement R sauvegardé
└── .Rhistory                      ← Historique des commandes R
```

---

## Lancement du projet

### Prérequis

- R 4.x installé — [r-project.org](https://www.r-project.org/)
- RStudio recommandé — [posit.co](https://posit.co/)

### Installation des packages

```r
install.packages(c("dplyr", "ggplot2", "tidyr", "readr"))
```

### Exécution

```r
# Lancer l'analyse complète depuis le script principal
source("projet_performance.R")

# Ou exécuter les modules indépendamment
source("Manipulation_Donnees.R")
source("Statistiques_Descriptives.R")
source("Detection_Outliers.R")
source("Visualisation.R")
source("Synthese.R")
```

---

## Résultats produits

L'analyse génère pour chaque variable clé (score de performance, heures travaillées) :

- Un **résumé statistique complet** (min, max, moyenne, médiane, écart-type, quartiles)
- Une **liste des employés outliers** identifiés avec leur écart aux bornes
- Des **visualisations exportables** (histogrammes, boxplots, pie charts, barplots)
- Un **rapport Word** consolidant toutes les conclusions (`rapport_performances_R.docx`)


## Technologies utilisées

| Outil | Rôle |
|---|---|
| R 4.x | Langage principal |
| dplyr | Manipulation et transformation des données |
| ggplot2 | Visualisation des distributions |
| readr | Import du fichier CSV |
| Visual studio code | Environnement de développement |

---

## Auteur

Projet réalisé dans le cadre d'une analyse RH exploratoire appliquée aux statistiques descriptives et à la détection d'anomalies.
