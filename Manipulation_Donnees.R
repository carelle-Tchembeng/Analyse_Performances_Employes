
# ÉTAPE 4 — Manipulation des données avec dplyr


#  4A. Créer des catégories pour mieux segmenter 

# On enrichit employes_clean avec de nouvelles colonnes calculées
employes_enrichi <- employes_clean %>%
  mutate(
    # Catégorie de performance simplifiée
    perf_categorie = case_when(
      PerformanceScore == "Exceeds"           ~ "Haute",
      PerformanceScore == "Fully Meets"       ~ "Standard",
      PerformanceScore == "Needs Improvement" ~ "En difficulté",
      PerformanceScore == "PIP"               ~ "En difficulté",
      TRUE                                    ~ "Inconnu"
    ),
    
    # Catégorie de salaire
    salary_categorie = case_when(
      Salary < 55502              ~ "Bas (Q1)",
      Salary >= 55502 & Salary <= 72036 ~ "Médian (Q1-Q3)",
      Salary > 72036 & Salary <= 96838  ~ "Élevé",
      Salary > 96838              ~ "Très élevé (outlier)"
    ),
    
    # Indicateur de retard
    a_des_retards = ifelse(DaysLateLast30 > 0, "Oui", "Non"),
    
    # Indicateur outlier engagement
    engagement_faible = ifelse(EngagementSurvey < 2.175, "Oui", "Non")
  )

# Vérification
head(employes_enrichi %>% 
       dplyr::select(Employee_Name, perf_categorie, 
                     salary_categorie, a_des_retards), 5)

# 4B. Profil complet des employés en difficulté

cat("=== PROFIL DES EMPLOYÉS EN DIFFICULTÉ ===\n\n")

employes_enrichi %>%
  filter(perf_categorie == "En difficulté") %>%
  summarise(
    Nb_total          = n(),
    Salary_moyen      = round(mean(Salary), 0),
    Engagement_moyen  = round(mean(EngagementSurvey), 2),
    Satisfaction_moy  = round(mean(EmpSatisfaction), 2),
    Absences_moy      = round(mean(Absences), 1),
    Retards_moy       = round(mean(DaysLateLast30), 2),
    Pct_avec_retards  = round(mean(a_des_retards == "Oui") * 100, 1)
  )

# 4C. Comparaison profil difficulté vs haute performance 

cat("=== COMPARAISON : EN DIFFICULTÉ vs HAUTE PERFORMANCE ===\n\n")

employes_enrichi %>%
  filter(perf_categorie %in% c("En difficulté", "Haute")) %>%
  group_by(perf_categorie) %>%
  summarise(
    Nb_employes       = n(),
    Salary_moyen      = round(mean(Salary), 0),
    Engagement_moyen  = round(mean(EngagementSurvey), 2),
    Satisfaction_moy  = round(mean(EmpSatisfaction), 2),
    Absences_moy      = round(mean(Absences), 1),
    Retards_moy       = round(mean(DaysLateLast30), 2),
    Pct_avec_retards  = round(mean(a_des_retards == "Oui") * 100, 1)
  )

# 4D. Tableau de bord complet par département 

cat("=== TABLEAU DE BORD PAR DÉPARTEMENT ===\n\n")

employes_enrichi %>%
  group_by(Department) %>%
  summarise(
    Nb_employes        = n(),
    Pct_haute_perf     = round(mean(perf_categorie == "Haute") * 100, 1),
    Pct_en_difficulte  = round(mean(perf_categorie == "En difficulté") * 100, 1),
    Engagement_moyen   = round(mean(EngagementSurvey), 2),
    Satisfaction_moy   = round(mean(EmpSatisfaction), 2),
    Salary_median      = round(median(Salary), 0),
    Absences_moy       = round(mean(Absences), 1),
    Pct_avec_retards   = round(mean(a_des_retards == "Oui") * 100, 1)
  ) %>%
  arrange(desc(Pct_en_difficulte))

# 4E. Répartition des catégories de performance par département

cat("=== PERFORMANCE PAR DÉPARTEMENT (détail) ===\n\n")

employes_enrichi %>%
  group_by(Department, PerformanceScore) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Department) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1)) %>%
  arrange(Department, desc(n))

# 4F. Visualisation — Heatmap engagement x performance x département

employes_enrichi %>%
  group_by(Department, perf_categorie) %>%
  summarise(
    Engagement_moyen = round(mean(EngagementSurvey), 2),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = perf_categorie, y = Department, fill = Engagement_moyen)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = Engagement_moyen), size = 4) +
  scale_fill_gradient(low = "#FCA5A5", high = "#22C55E") +
  labs(
    title    = "Engagement moyen par département et niveau de performance",
    subtitle = "Rouge = faible engagement | Vert = fort engagement",
    x        = "Catégorie de performance",
    y        = "Département",
    fill     = "Engagement"
  ) +
  theme_minimal()

#  4G. Visualisation — Comparaison des profils en difficulté vs haute perf 

# Préparer les données pour le graphique radar-like en barres
comparaison <- employes_enrichi %>%
  filter(perf_categorie %in% c("En difficulté", "Haute")) %>%
  group_by(perf_categorie) %>%
  summarise(
    Engagement     = round(mean(EngagementSurvey), 2),
    Satisfaction   = round(mean(EmpSatisfaction), 2),
    Absences_norm  = round(mean(Absences) / 20 * 5, 2),  # normalisé sur 5
    Retards_norm   = round(mean(DaysLateLast30) / 6 * 5, 2) # normalisé sur 5
  )

# Transformer en format long pour ggplot
library(tidyr)

comparaison_long <- comparaison %>%
  pivot_longer(cols = -perf_categorie,
               names_to  = "indicateur",
               values_to = "valeur")

ggplot(comparaison_long, 
       aes(x = indicateur, y = valeur, fill = perf_categorie)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = valeur), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("En difficulté" = "#EF4444", 
                                "Haute"         = "#22C55E")) +
  labs(
    title    = "Comparaison des profils : haute performance vs en difficulté",
    subtitle = "Absences et retards normalisés sur une échelle de 0 à 5",
    x        = "Indicateur",
    y        = "Valeur moyenne",
    fill     = "Catégorie"
  ) +
  theme_minimal() +
  ylim(0, 6)

# 4H. Top 10 employés les plus à risque

cat("=== TOP 10 EMPLOYÉS LES PLUS À RISQUE ===\n\n")

employes_enrichi %>%
  filter(perf_categorie == "En difficulté") %>%
  mutate(
    score_risque = round(
      (6 - EngagementSurvey) +          # faible engagement = risque élevé
      DaysLateLast30 +                   # retards = risque
      (Absences / 20 * 5),               # absences normalisées
      2)
  ) %>%
  dplyr::select(Employee_Name, Department, PerformanceScore,
                EngagementSurvey, DaysLateLast30, 
                Absences, score_risque) %>%
  arrange(desc(score_risque)) %>%
  head(10)

