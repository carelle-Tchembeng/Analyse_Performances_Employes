
# ÉTAPE 2 — Visualisation des distributions

# 2A. Histogramme — Distribution des salaires 

ggplot(employes_clean, aes(x = Salary)) +
  geom_histogram(bins = 30, fill = "#3B82F6", color = "white") +
  geom_vline(aes(xintercept = mean(Salary)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(Salary)), 
             color = "green", linetype = "dashed", linewidth = 1) +
  labs(
    title    = "Distribution des salaires",
    subtitle = "Ligne rouge = moyenne | Ligne verte = médiane",
    x        = "Salaire ($)",
    y        = "Nombre d'employés"
  ) +
  theme_minimal()

  # 2B. Histogramme — Distribution de l'engagement 

ggplot(employes_clean, aes(x = EngagementSurvey)) +
  geom_histogram(bins = 20, fill = "#10B981", color = "white") +
  geom_vline(aes(xintercept = mean(EngagementSurvey)),
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(EngagementSurvey)),
             color = "orange", linetype = "dashed", linewidth = 1) +
  labs(
    title    = "Distribution du score d'engagement",
    subtitle = "Ligne rouge = moyenne | Ligne orange = médiane",
    x        = "Score d'engagement (1 à 5)",
    y        = "Nombre d'employés"
  ) +
  theme_minimal()

  # --- 2C. Histogramme — Distribution des absences ---

ggplot(employes_clean, aes(x = Absences)) +
  geom_histogram(bins = 20, fill = "#F59E0B", color = "white") +
  geom_vline(aes(xintercept = mean(Absences)),
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(Absences)),
             color = "blue", linetype = "dashed", linewidth = 1) +
  labs(
    title    = "Distribution des absences",
    subtitle = "Ligne rouge = moyenne | Ligne bleue = médiane",
    x        = "Nombre d'absences",
    y        = "Nombre d'employés"
  ) +
  theme_minimal()

  # --- 2D. Boxplot — Salaire par département ---

ggplot(employes_clean, aes(x = Department, y = Salary, fill = Department)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  labs(
    title    = "Distribution des salaires par département",
    subtitle = "Les points rouges sont des outliers potentiels",
    x        = "Département",
    y        = "Salaire ($)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 25, hjust = 1)
  )

  # --- 2E. Boxplot — Engagement par niveau de performance ---

ggplot(employes_clean, aes(x = PerformanceScore, y = EngagementSurvey, 
                            fill = PerformanceScore)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  labs(
    title = "Engagement selon le niveau de performance",
    x     = "Niveau de performance",
    y     = "Score d'engagement"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

  # --- 2F. Pie chart — Répartition des niveaux de performance ---

# Préparer les données pour le pie chart
perf_counts <- employes_clean %>%
  group_by(PerformanceScore) %>%
  summarise(n = n()) %>%
  mutate(
    pourcentage = round(n / sum(n) * 100, 1),
    label       = paste0(PerformanceScore, "\n", pourcentage, "%")
  )

ggplot(perf_counts, aes(x = "", y = n, fill = PerformanceScore)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), size = 3.5) +
  labs(title = "Répartition des niveaux de performance") +
  theme_void() +
  theme(legend.position = "none")

  # --- 2G. Diagramme en barres — Salaire moyen par département ---

employes_clean %>%
  group_by(Department) %>%
  summarise(Salaire_moyen = round(mean(Salary), 0)) %>%
  arrange(desc(Salaire_moyen)) %>%
  ggplot(aes(x = reorder(Department, Salaire_moyen), 
             y = Salaire_moyen, 
             fill = Department)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Salaire_moyen/1000, 0), "k$")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Salaire moyen par département",
    x     = "Département",
    y     = "Salaire moyen ($)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 280000)

