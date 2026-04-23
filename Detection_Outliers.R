
# ÉTAPE 3 — Détection et analyse des outliers (règle 1.5 × IQR)


#  3A. Fonction universelle de détection des outliers 

# On crée une fonction réutilisable qui :
# 1. Calcule les seuils bas et haut
# 2. Identifie les outliers
# 3. Affiche un résumé clair

detecter_outliers <- function(donnees, colonne, nom_variable) {
  
  x   <- donnees[[colonne]]   # extrait la colonne
  
  Q1  <- quantile(x, 0.25)
  Q3  <- quantile(x, 0.75)
  IQR_val <- IQR(x)
  
  seuil_bas  <- Q1 - 1.5 * IQR_val
  seuil_haut <- Q3 + 1.5 * IQR_val
  
  outliers <- donnees[x < seuil_bas | x > seuil_haut, ]
  
  cat("======", nom_variable, "======\n")
  cat("  Q1            :", Q1, "\n")
  cat("  Q3            :", Q3, "\n")
  cat("  IQR           :", IQR_val, "\n")
  cat("  Seuil bas     :", seuil_bas, "\n")
  cat("  Seuil haut    :", seuil_haut, "\n")
  cat("  Nb outliers   :", nrow(outliers), "\n\n")
  
  return(outliers)
}

# --- 3B. Application sur chaque variable numérique ---

outliers_salary <- detecter_outliers(employes_clean, "Salary", "SALARY")

outliers_engagement <- detecter_outliers(employes_clean, "EngagementSurvey", 
                                          "ENGAGEMENT SURVEY")

outliers_absences <- detecter_outliers(employes_clean, "Absences", 
                                        "ABSENCES")

outliers_retards <- detecter_outliers(employes_clean, "DaysLateLast30", 
                                       "JOURS DE RETARD")

# --- 3C. Afficher le détail des outliers salary ---

cat("=== Détail des outliers Salary ===\n")
outliers_salary %>%
  dplyr::select(Employee_Name, Department, Salary, PerformanceScore) %>%
  arrange(desc(Salary))

# --- 3D. Afficher le détail des outliers engagement ---

cat("=== Détail des outliers Engagement ===\n")
outliers_engagement %>%
  dplyr::select(Employee_Name, Department, EngagementSurvey, PerformanceScore) %>%
  arrange(EngagementSurvey)

# --- 3E. Visualisation — Boxplot annoté avec les seuils IQR ---

# Calcul des seuils pour le graphique
Q1_sal  <- quantile(employes_clean$Salary, 0.25)
Q3_sal  <- quantile(employes_clean$Salary, 0.75)
IQR_sal <- IQR(employes_clean$Salary)
seuil_haut_sal <- Q3_sal + 1.5 * IQR_sal
seuil_bas_sal  <- Q1_sal - 1.5 * IQR_sal

ggplot(employes_clean, aes(x = "", y = Salary)) +
  geom_boxplot(fill = "#3B82F6", outlier.colour = "red", 
               outlier.size = 3, width = 0.4) +
  geom_hline(yintercept = seuil_haut_sal, 
             color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = seuil_bas_sal,  
             color = "red", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = 1.3, y = seuil_haut_sal + 5000,
           label = paste0("Seuil haut : ", round(seuil_haut_sal, 0), "$"),
           color = "red", size = 3.5) +
  annotate("text", x = 1.3, y = seuil_bas_sal - 5000,
           label = paste0("Seuil bas : ", round(seuil_bas_sal, 0), "$"),
           color = "red", size = 3.5) +
  labs(
    title    = "Détection des outliers — Salary",
    subtitle = "Points rouges = outliers | Lignes rouges = seuils 1.5 × IQR",
    x        = "",
    y        = "Salaire ($)"
  ) +
  theme_minimal()

# --- 3F. Visualisation — Outliers retards (cas particulier IQR = 0) ---

# Quand IQR = 0, la règle 1.5 x IQR ne fonctionne pas
# On utilise alors uniquement le seuil : toute valeur > 0 est suspecte

cat("=== Cas particulier : DaysLateLast30 ===\n")
cat("IQR = 0 → la règle 1.5 × IQR donne seuil haut = 0\n")
cat("Interprétation : tout retard > 0 est statistiquement inhabituel\n\n")

employes_clean %>%
  filter(DaysLateLast30 > 0) %>%
  dplyr::select(Employee_Name, Department, DaysLateLast30, 
                PerformanceScore, Absences) %>%
  arrange(desc(DaysLateLast30))

# --- 3G. Analyse croisée — Les outliers salary sont-ils de bons performers ? ---

employes_clean %>%
  mutate(est_outlier_salary = Salary > seuil_haut_sal) %>%
  group_by(est_outlier_salary, PerformanceScore) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(est_outlier_salary, desc(n))

