# ÉTAPE 1 — Statistiques descriptives

# 1A. Indicateurs sur les variables NUMÉRIQUES 

# On sélectionne uniquement les colonnes numériques qui nous intéressent
variables_numeriques <- employes_clean %>%
  dplyr::select(Salary, EngagementSurvey, EmpSatisfaction,
                DaysLateLast30, Absences, SpecialProjectsCount)

# Résumé complet automatique
summary(variables_numeriques)

# 1B. Calcul manuel des indicateurs clés (pour bien comprendre)

# Fonction personnalisée qui calcule tout d'un coup
mes_stats <- function(x) {
  cat("  Moyenne       :", round(mean(x), 2), "\n")
  cat("  Médiane       :", round(median(x), 2), "\n")
  cat("  Écart-type    :", round(sd(x), 2), "\n")
  cat("  Variance      :", round(var(x), 2), "\n")
  cat("  Q1 (25%)      :", quantile(x, 0.25), "\n")
  cat("  Q3 (75%)      :", quantile(x, 0.75), "\n")
  cat("  IQR           :", IQR(x), "\n")
  cat("  Min           :", min(x), "\n")
  cat("  Max           :", max(x), "\n")
}

# Application sur chaque variable
cat("====== SALARY ======\n")
mes_stats(employes_clean$Salary)

cat("\n====== ENGAGEMENT SURVEY ======\n")
mes_stats(employes_clean$EngagementSurvey)

cat("\n====== SATISFACTION EMPLOYÉ ======\n")
mes_stats(employes_clean$EmpSatisfaction)

cat("\n====== JOURS DE RETARD ======\n")
mes_stats(employes_clean$DaysLateLast30)

cat("\n====== ABSENCES ======\n")
mes_stats(employes_clean$Absences)

# --- 1C. Mode sur PerformanceScore (variable texte) ---

# R n'a pas de fonction mode() native pour les textes, on la crée
mon_mode <- function(x) {
  tableau <- table(x)        # compte chaque valeur
  names(which.max(tableau))  # retourne la valeur la plus fréquente
}

cat("\n====== PERFORMANCE SCORE ======\n")
cat("Mode (valeur la plus fréquente) :", mon_mode(employes_clean$PerformanceScore), "\n")

# Répartition complète des scores
cat("\nRépartition des scores :\n")
table(employes_clean$PerformanceScore)

# En pourcentage
cat("\nEn pourcentage :\n")
round(prop.table(table(employes_clean$PerformanceScore)) * 100, 1)

# --- 1D. Statistiques par département ---

employes_clean %>%
  group_by(Department) %>%
  summarise(
    Nb_employes       = n(),
    Salaire_moyen     = round(mean(Salary), 0),
    Salaire_median    = round(median(Salary), 0),
    Engagement_moyen  = round(mean(EngagementSurvey), 2),
    Absences_moyennes = round(mean(Absences), 1)
  ) %>%
  arrange(desc(Salaire_moyen))

