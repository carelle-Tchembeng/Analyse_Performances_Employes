
# ÉTAPE 0 — Chargement et exploration initiale des données

library(ggplot2)
library(dplyr)

# 2. Charger le fichier CSV
employes <- read.csv("C:/Users/carel/Desktop/Portfolio/Projet2/DataPerformances.csv", 
                     header = TRUE, 
                     sep = ",", 
                     stringsAsFactors = FALSE,
                     fileEncoding = "UTF-8-BOM")

# 3. Dimensions du tableau
cat("Nombre de lignes (employés) :", nrow(employes), "\n")
cat("Nombre de colonnes (variables) :", ncol(employes), "\n")

# 4. Aperçu des premières lignes
head(employes, 5)

# 5. Noms de toutes les colonnes
colnames(employes)

# 6. Types de chaque colonne
str(employes)

# 7. Résumé statistique rapide
summary(employes)

# 8. Vérifier les valeurs manquantes par colonne
colSums(is.na(employes))

# 9. Garder uniquement les colonnes utiles au projet
employes_clean <- employes %>%
  dplyr::select(Employee_Name, Department, Salary,
                PerformanceScore, EngagementSurvey,
                EmpSatisfaction, DaysLateLast30,
                Absences, SpecialProjectsCount,
                EmploymentStatus)

# 10. Vérifier le résultat
head(employes_clean, 5)
cat("Dimensions du tableau nettoyé :", nrow(employes_clean), "lignes x", ncol(employes_clean), "colonnes\n")

# 11. Nettoyer les espaces superflus dans Department
employes_clean <- employes_clean %>%
  mutate(Department = trimws(Department))

# Vérifier les départements distincts
unique(employes_clean$Department)