
# ÉTAPE 5 — Synthèse et conclusions

#  5A. Tableau de bord final — Vue globale de l'entreprise 

cat("========================================\n")
cat("   TABLEAU DE BORD — PERFORMANCES RH   \n")
cat("========================================\n\n")

cat("--- Vue générale ---\n")
cat("Effectif total         :", nrow(employes_enrichi), "employés\n")
cat("Départements           :", n_distinct(employes_enrichi$Department), "\n")
cat("Salary médian global   :", median(employes_enrichi$Salary), "$\n")
cat("Engagement moyen       :", round(mean(employes_enrichi$EngagementSurvey), 2), "/ 5\n")
cat("Satisfaction moyenne   :", round(mean(employes_enrichi$EmpSatisfaction), 2), "/ 5\n\n")

cat("--- Répartition des performances ---\n")
perf_table <- round(prop.table(table(employes_enrichi$PerformanceScore)) * 100, 1)
for(i in 1:length(perf_table)) {
  cat(sprintf("  %-22s : %5.1f%%\n", names(perf_table)[i], perf_table[i]))
}

cat("\n--- Employés en difficulté ---\n")
nb_difficulte <- sum(employes_enrichi$perf_categorie == "En difficulté")
cat("Nb en difficulté       :", nb_difficulte, "employés\n")
cat("% de l'effectif        :", round(nb_difficulte / nrow(employes_enrichi) * 100, 1), "%\n")
cat("Engagement moyen       :", 
    round(mean(employes_enrichi$EngagementSurvey[employes_enrichi$perf_categorie == "En difficulté"]), 2), "\n")
cat("% avec retards         : 96.8%\n")
cat("Département le + touché: Production (23 cas sur 31)\n")


# --- 5B. Graphique de synthèse 1 — Score de risque par département 

employes_enrichi %>%
  filter(perf_categorie == "En difficulté") %>%
  mutate(score_risque = round(
    (6 - EngagementSurvey) + DaysLateLast30 + (Absences / 20 * 5), 2)
  ) %>%
  group_by(Department) %>%
  summarise(
    Nb_cas           = n(),
    Score_risque_moy = round(mean(score_risque), 2),
    Engagement_moy   = round(mean(EngagementSurvey), 2)
  ) %>%
  ggplot(aes(x = reorder(Department, Score_risque_moy),
             y = Score_risque_moy,
             fill = Score_risque_moy)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("Score: ", Score_risque_moy,
                               "\n(", Nb_cas, " cas)")),
            hjust = -0.05, size = 3.2) +
  scale_fill_gradient(low = "#FCD34D", high = "#DC2626") +
  coord_flip() +
  expand_limits(y = 18) +
  labs(
    title    = "Score de risque moyen par département",
    subtitle = "Calculé sur les employés en difficulté uniquement",
    x        = "Département",
    y        = "Score de risque moyen",
    fill     = "Risque"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

  
# --- 5C. Graphique de synthèse 2 — Matrice engagement vs performance ---

ggplot(employes_enrichi,
       aes(x = EngagementSurvey,
           y = EmpSatisfaction,
           color = perf_categorie,
           size  = perf_categorie)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c(
    "En difficulté" = "#EF4444",
    "Standard"      = "#3B82F6",
    "Haute"         = "#22C55E"
  )) +
  scale_size_manual(values = c(
    "En difficulté" = 4,
    "Standard"      = 2,
    "Haute"         = 3
  )) +
  geom_vline(xintercept = 2.175, color = "red",
             linetype = "dashed", linewidth = 0.7) +
  annotate("text", x = 2.3, y = 1.2,
           label = "Seuil outlier\nengagement",
           color = "red", size = 3) +
  labs(
    title    = "Matrice Engagement × Satisfaction colorée par performance",
    subtitle = "Rouge = En difficulté | Bleu = Standard | Vert = Haute performance",
    x        = "Score d'engagement (1 à 5)",
    y        = "Score de satisfaction (1 à 5)",
    color    = "Performance",
    size     = "Performance"
  ) +
  theme_minimal()

# --- 5D. Graphique de synthèse 3 — Pyramide des performances par département ---

employes_enrichi %>%
  group_by(Department, perf_categorie) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Department) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = reorder(Department, pct),
             y = pct,
             fill = perf_categorie)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", fontface = "bold") +
  scale_fill_manual(values = c(
    "En difficulté" = "#EF4444",
    "Standard"      = "#3B82F6",
    "Haute"         = "#22C55E"
  )) +
  coord_flip() +
  labs(
    title    = "Répartition des niveaux de performance par département",
    subtitle = "Rouge = En difficulté | Bleu = Standard | Vert = Haute",
    x        = "Département",
    y        = "Pourcentage (%)",
    fill     = "Performance"
  ) +
  theme_minimal()

# --- 5E. Graphique de synthèse 4 — Les 10 employés les plus à risque ---

employes_enrichi %>%
  filter(perf_categorie == "En difficulté") %>%
  mutate(score_risque = round(
    (6 - EngagementSurvey) + DaysLateLast30 + (Absences / 20 * 5), 2)
  ) %>%
  arrange(desc(score_risque)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(Employee_Name, score_risque),
             y = score_risque,
             fill = Department)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(score_risque, 1)),
            hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title    = "Top 10 employés les plus à risque",
    subtitle = "Score combiné : désengagement + retards + absences",
    x        = "Employé",
    y        = "Score de risque",
    fill     = "Département"
  ) +
  theme_minimal() +
  ylim(0, 17)

# --- 5F. Conclusions chiffrées finales ---

cat("\n")
cat("================================================\n")
cat("   CONCLUSIONS ET RECOMMANDATIONS FINALES      \n")
cat("================================================\n\n")

cat("FINDING 1 — ENGAGEMENT : PRÉDICTEUR N°1 DE LA PERFORMANCE\n")
cat("----------------------------------------------------------\n")
eng_haute <- round(mean(employes_enrichi$EngagementSurvey[
  employes_enrichi$perf_categorie == "Haute"]), 2)
eng_diff  <- round(mean(employes_enrichi$EngagementSurvey[
  employes_enrichi$perf_categorie == "En difficulté"]), 2)
cat(sprintf("  Engagement moyen Haute perf    : %.2f / 5\n", eng_haute))
cat(sprintf("  Engagement moyen En difficulté : %.2f / 5\n", eng_diff))
cat(sprintf("  Écart                          : %.2f points\n\n", eng_haute - eng_diff))

cat("FINDING 2 — RETARDS : SIGNAL COMPORTEMENTAL UNIVERSEL\n")
cat("------------------------------------------------------\n")
cat("  96.8% des employés en difficulté ont des retards\n")
cat("  vs une minorité parmi les Standard et Haute perf\n\n")

cat("FINDING 3 — ABSENCES : FAUX INDICATEUR DE PERFORMANCE\n")
cat("------------------------------------------------------\n")
abs_haute <- round(mean(employes_enrichi$Absences[
  employes_enrichi$perf_categorie == "Haute"]), 1)
abs_diff  <- round(mean(employes_enrichi$Absences[
  employes_enrichi$perf_categorie == "En difficulté"]), 1)
cat(sprintf("  Absences moy Haute perf    : %.1f jours\n", abs_haute))
cat(sprintf("  Absences moy En difficulté : %.1f jours\n", abs_diff))
cat("  → Différence négligeable : les absences ne prédisent pas\n")
cat("    la performance\n\n")

cat("FINDING 4 — SALAIRES : OUTLIERS CONCENTRÉS EN IT/IS\n")
cat("----------------------------------------------------\n")
cat("  29 outliers salary sur 311 employés (9.3%)\n")
cat("  Seuil haut : 96 838 $\n")
cat("  Cas critique : Monroe Peter (157 000$ / Needs Improvement)\n\n")

cat("FINDING 5 — DÉPARTEMENTS À RISQUE\n")
cat("----------------------------------\n")
cat("  Sales       : 12.9% de PIP — taux le plus élevé\n")
cat("  Production  : 23 cas sur 31 en difficulté (74%)\n")
cat("  Soft. Eng.  : meilleur taux de Exceeds (18.2%)\n\n")

cat("RECOMMANDATIONS\n")
cat("---------------\n")
cat("  R1 → Mettre en place des entretiens d'engagement\n")
cat("       ciblés pour les 9 outliers d'engagement\n\n")
cat("  R2 → Surveiller en priorité les employés avec\n")
cat("       retards chroniques (> 3 jours / 30)\n\n")
cat("  R3 → Investiguer les causes du faible engagement\n")
cat("       dans le département Sales\n\n")
cat("  R4 → Ne pas utiliser les absences seules comme\n")
cat("       critère d'évaluation de la performance\n\n")
cat("  R5 → Traiter le cas Monroe Peter en priorité :\n")
cat("       coût élevé (157k$) + performance insuffisante\n\n")
