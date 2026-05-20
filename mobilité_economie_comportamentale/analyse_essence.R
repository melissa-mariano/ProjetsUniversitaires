# ==============================================================================
#  PROJET : MOBILITÉ & ÉCONOMIE COMPORTEMENTALE (FRANCE 2019-2026)
#  Prix de l'essence & habitudes de déplacement
#  SCRIPT : DATA STORYTELLING LINÉAIRE (SQ1 → SQ2 → SQ3)
#  Analyses : Seuil de Rupture | Report Modal | Élasticités | Log-linéaire
# ==============================================================================

# ── 0. PACKAGES ────────────────────────────────────────────────────────────────
packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "purrr",
              "broom", "knitr", "scales", "ggpmisc", "patchwork", "gt")

installed  <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]
if (length(to_install) > 0) install.packages(to_install, repos = "https://cran.r-project.org")
invisible(lapply(packages, library, character.only = TRUE))

cat("\n✅ Tous les packages chargés.\n")


# ── 1. CHARGEMENT & PRÉPARATION DES DONNÉES ────────────────────────────────────
FICHIER <- "chaker_big_boss_final_bonne_valeur___1_.xlsx"
df_raw  <- read_excel(FICHIER, sheet = "DATA")
names(df_raw) <- make.names(names(df_raw))

# ── df_var : dados em VARIAÇÕES % (elasticidades discretas : SQ1, SQ2, SQ3-A)
df_var <- df_raw %>%
  filter(!is.na(Date), !is.na(Zone), Zone %in% c("Urbaine", "Périurbaine", "Rurale")) %>%
  mutate(
    Date       = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d"),
    Zone       = factor(Zone, levels = c("Urbaine", "Périurbaine", "Rurale")),
    Var_SP95   = as.numeric(Var.SP95..),
    Var_Trafic = as.numeric(Var.Trafic..),
    Var_TC     = as.numeric(Var.TC..),
    Var_Gazole = as.numeric(Var.Gazole..),
    Prix_SP95  = as.numeric(Prix_SP95),
    Trafic     = as.numeric(Trafic.Routier),
    Voyageurs  = as.numeric(Voyageurs.TC)
  ) %>%
  filter(!is.na(Var_SP95), !is.na(Var_Trafic), !is.na(Var_TC))

# ── df_abs : dados em NÍVEIS ABSOLUTOS (série temporal & log-linéaire : SQ3-B)
df_abs <- df_raw %>%
  filter(!is.na(Date), Zone %in% c("Urbaine", "Périurbaine", "Rurale")) %>%
  mutate(
    Date        = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d"),
    Zone        = factor(Zone, levels = c("Urbaine", "Périurbaine", "Rurale")),
    Prix_SP95   = as.numeric(Prix_SP95),
    Prix_Gazole = as.numeric(Prix.Gazole),
    Trafic      = as.numeric(Trafic.Routier),
    Voyageurs   = as.numeric(Voyageurs.TC)
  ) %>%
  filter(!is.na(Prix_SP95), !is.na(Trafic), Trafic > 0, Prix_SP95 > 0)

couleurs_zone <- c("Urbaine" = "#2196F3", "Périurbaine" = "#FF9800", "Rurale" = "#4CAF50")

cat(sprintf("📊 Données chargées : %d obs. (df_var) | %d obs. (df_abs)\n",
            nrow(df_var), nrow(df_abs)))
cat(sprintf("📅 Période : %s → %s  |  Zones : %s\n",
            format(min(df_abs$Date), "%Y-%m"),
            format(max(df_abs$Date), "%Y-%m"),
            paste(levels(df_abs$Zone), collapse = ", ")))


# ══════════════════════════════════════════════════════════════════════════════
#  INTRODUCTION : CONTEXTE HISTORIQUE GLOBAL
# ══════════════════════════════════════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  INTRODUCTION — Évolution temporelle historique\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

df_glob <- df_abs %>%
  group_by(Date) %>%
  summarise(
    Prix_SP95 = mean(Prix_SP95),
    Trafic    = mean(Trafic),
    Voyageurs = mean(Voyageurs),
    .groups   = "drop"
  )

coef_ax <- max(df_glob$Trafic) / max(df_glob$Prix_SP95)

g1 <- ggplot(df_glob, aes(x = Date)) +
  geom_line(aes(y = Trafic,              colour = "Trafic routier"), linewidth = 1.0) +
  geom_line(aes(y = Prix_SP95 * coef_ax, colour = "Prix SP95"),     linewidth = 1.0, linetype = "dashed") +
  scale_y_continuous(
    name     = "Trafic routier (véh.)",
    sec.axis = sec_axis(~./coef_ax, name = "Prix SP95 (€/L)",
                        labels = label_number(suffix = "€"))
  ) +
  scale_colour_manual(values = c("Trafic routier" = "#2196F3", "Prix SP95" = "#F44336")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "G1 — Évolution du prix SP95 et du trafic routier (2019-2026)",
    subtitle = "Données agrégées toutes zones confondues — axe droit : prix SP95",
    x = NULL, colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")


# ══════════════════════════════════════════════════════════════════════════════
#  SQ1 — SEUIL DE RUPTURE PSYCHOLOGIQUE
#  À partir de quel prix observe-t-on une baisse significative du trafic ?
# ══════════════════════════════════════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  SQ1 — SEUIL DE RUPTURE PSYCHOLOGIQUE\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Stratification en 5 faixas de prix + calcul de l'élasticité par observation
df_seuil <- df_var %>%
  mutate(
    Faix_Prix = case_when(
      Prix_SP95 < 1.50                       ~ "F1: <1.50€",
      Prix_SP95 >= 1.50 & Prix_SP95 < 1.70   ~ "F2: 1.50-1.70€",
      Prix_SP95 >= 1.70 & Prix_SP95 < 1.90   ~ "F3: 1.70-1.90€",
      Prix_SP95 >= 1.90 & Prix_SP95 < 2.10   ~ "F4: 1.90-2.10€",
      TRUE                                   ~ "F5: ≥2.10€"
    ),
    Faix_Prix    = factor(Faix_Prix, levels = c(
      "F1: <1.50€", "F2: 1.50-1.70€", "F3: 1.70-1.90€",
      "F4: 1.90-2.10€", "F5: ≥2.10€"
    )),
    epsilon_SP95 = Var_Trafic / Var_SP95
  ) %>%
  filter(abs(epsilon_SP95) < 50)

# Élasticité moyenne + IC 95% par faixa et par zone
seuil_table <- df_seuil %>%
  group_by(Zone, Faix_Prix) %>%
  summarise(
    n_obs    = n(),
    Prix_moy = mean(Prix_SP95),
    eps_moy  = mean(epsilon_SP95, na.rm = TRUE),
    eps_med  = median(epsilon_SP95, na.rm = TRUE),
    eps_sd   = sd(epsilon_SP95, na.rm = TRUE),
    Var_Traf = mean(Var_Trafic, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    IC_inf = eps_moy - 1.96 * eps_sd / sqrt(n_obs),
    IC_sup = eps_moy + 1.96 * eps_sd / sqrt(n_obs)
  )

# Détection numérique du seuil : plus grand saut d'élasticité entre faixas adjacentes
seuil_rupture <- seuil_table %>%
  group_by(Zone) %>%
  arrange(Faix_Prix) %>%
  mutate(
    delta_eps = eps_moy - lag(eps_moy),
    Rupture   = abs(delta_eps) > 0.15
  ) %>%
  filter(!is.na(delta_eps)) %>%
  summarise(
    Faix_Rupture = Faix_Prix[which.max(abs(delta_eps))],
    Delta_max    = round(max(abs(delta_eps), na.rm = TRUE), 4),
    .groups      = "drop"
  )

cat("\n📋 Élasticités par faixa de prix et zone :\n")
seuil_table %>%
  select(Zone, Faix_Prix, n_obs, Prix_moy, eps_moy, eps_med, IC_inf, IC_sup, Var_Traf) %>%
  mutate(across(where(is.numeric), ~round(.x, 4))) %>%
  print(n = Inf)

cat("\n🔴 SEUIL DE RUPTURE DÉTECTÉ NUMÉRIQUEMENT :\n")
print(seuil_rupture)

g3 <- ggplot(seuil_table, aes(x = Faix_Prix, y = eps_moy, fill = Zone)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_errorbar(
    aes(ymin = IC_inf, ymax = IC_sup),
    position = position_dodge(0.9), width = 0.25, linewidth = 0.7
  ) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_vline(xintercept = 3.5, linetype = "dashed", colour = "#F44336", linewidth = 1) +
  annotate("text", x = 3.55, y = max(seuil_table$eps_moy, na.rm = TRUE) * 0.85,
           label = "<- Seuil detecte", hjust = 0, size = 3.5, colour = "#F44336") +
  scale_fill_manual(values = couleurs_zone) +
  labs(
    title    = "G3 - SQ1 : Elasticite par faixa de prix - Seuil de rupture psychologique",
    subtitle = "Rupture = plus grand saut d'elasticite entre deux faixas adjacentes  |  IC 95%",
    x = "Faixa de prix SP95", y = "Elasticite moyenne (epsilon)", fill = "Zone"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))


# ══════════════════════════════════════════════════════════════════════════════
#  SQ2 — ÉLASTICITÉ CROISÉE & REPORT MODAL
#  La hausse du prix corrèle-t-elle avec une augmentation des TC ?
# ══════════════════════════════════════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  SQ2 - ELASTICITE CROISEE & REPORT MODAL\n")
cat("  epsilon_croisee = DeltaTC% / DeltaPrix_SP95%\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

df_cross <- df_var %>%
  mutate(
    epsilon_croisee = Var_TC / Var_SP95,
    Faix_Prix = case_when(
      Prix_SP95 < 1.50                       ~ "F1: <1.50EUR",
      Prix_SP95 >= 1.50 & Prix_SP95 < 1.70   ~ "F2: 1.50-1.70EUR",
      Prix_SP95 >= 1.70 & Prix_SP95 < 1.90   ~ "F3: 1.70-1.90EUR",
      Prix_SP95 >= 1.90 & Prix_SP95 < 2.10   ~ "F4: 1.90-2.10EUR",
      TRUE                                   ~ "F5: >=2.10EUR"
    ),
    Faix_Prix = factor(Faix_Prix, levels = c(
      "F1: <1.50EUR", "F2: 1.50-1.70EUR", "F3: 1.70-1.90EUR",
      "F4: 1.90-2.10EUR", "F5: >=2.10EUR"
    ))
  ) %>%
  filter(abs(epsilon_croisee) < 50)

# Agrégation par zone avec IC 95% et interprétation automatique
cross_zone <- df_cross %>%
  group_by(Zone) %>%
  summarise(
    n             = n(),
    eps_crois_moy = mean(epsilon_croisee, na.rm = TRUE),
    eps_crois_med = median(epsilon_croisee, na.rm = TRUE),
    eps_crois_sd  = sd(epsilon_croisee, na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  mutate(
    IC_inf = eps_crois_moy - 1.96 * eps_crois_sd / sqrt(n),
    IC_sup = eps_crois_moy + 1.96 * eps_crois_sd / sqrt(n),
    Interpretation = case_when(
      eps_crois_moy > 0.10 ~ "Biens substituts -> Report modal avere",
      eps_crois_moy > 0    ~ "Legere substituabilite",
      TRUE                 ~ "Biens independants -> Pas de report modal"
    )
  )

cat("\n Tableau synthese SQ2 :\n")
cross_zone %>%
  select(Zone, eps_crois_moy, eps_crois_med, IC_inf, IC_sup, Interpretation) %>%
  rename(
    "epsilon croisee (moy)" = eps_crois_moy,
    "epsilon croisee (med)" = eps_crois_med,
    "IC 95% inf"            = IC_inf,
    "IC 95% sup"            = IC_sup
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 4))) %>%
  print()

# Elasticité croisée par faixa (est-ce que le report s'intensifie avec le prix ?)
cross_faix <- df_cross %>%
  group_by(Zone, Faix_Prix) %>%
  summarise(eps_croisee = mean(epsilon_croisee, na.rm = TRUE), .groups = "drop")

cat("\n Elasticite croisee par faixa de prix :\n")
cross_faix %>% mutate(across(where(is.numeric), ~round(.x, 4))) %>% print(n = Inf)

g4 <- ggplot(cross_zone, aes(x = Zone, y = eps_crois_moy, fill = Zone)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2, linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid") +
  scale_fill_manual(values = couleurs_zone) +
  labs(
    title    = "G4 - SQ2 : Elasticite croisee - Report modal vers les TC",
    subtitle = "epsilon > 0 -> Substitution  |  epsilon ~ 0 -> Independance  |  IC 95%",
    x = NULL, y = "Elasticite croisee", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

g6 <- ggplot(cross_faix, aes(x = Faix_Prix, y = eps_croisee, colour = Zone, group = Zone)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_colour_manual(values = couleurs_zone) +
  labs(
    title    = "G6 - SQ2 : Intensite du report modal selon le niveau de prix",
    subtitle = "Le report augmente-t-il quand le prix franchit le seuil ?",
    x = "Faixa de prix SP95", y = "Elasticite croisee", colour = "Zone"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))


# ══════════════════════════════════════════════════════════════════════════════
#  SQ3 — FACTEUR GÉOGRAPHIQUE (Contrainte vs Choix)
#  L'élasticité est-elle identique en zone rurale et en zone urbaine ?
# ══════════════════════════════════════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  SQ3 - FACTEUR GEOGRAPHIQUE\n")
cat("  A. Elasticite directe discrete par zone\n")
cat("  B. Regression log-lineaire structurelle (Nijkamp & Gelman)\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# ── SQ3-A : Elasticite directe discrete ──────────────────────────────────────
elast_zone <- df_seuil %>%
  group_by(Zone) %>%
  summarise(
    n            = n(),
    eps_moy_SP95 = mean(epsilon_SP95, na.rm = TRUE),
    eps_med_SP95 = median(epsilon_SP95, na.rm = TRUE),
    eps_sd_SP95  = sd(epsilon_SP95, na.rm = TRUE),
    eps_moy_Gaz  = mean(Var_Trafic / Var_Gazole, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  mutate(
    IC_inf = eps_moy_SP95 - 1.96 * eps_sd_SP95 / sqrt(n),
    IC_sup = eps_moy_SP95 + 1.96 * eps_sd_SP95 / sqrt(n),
    Interpretation = case_when(
      abs(eps_moy_SP95) < 0.15 ~ "Tres inelastique (dependance forte)",
      abs(eps_moy_SP95) < 0.40 ~ "Peu elastique",
      abs(eps_moy_SP95) < 1.00 ~ "Elastique modere",
      TRUE                     ~ "Tres elastique (sensibilite forte)"
    )
  )

cat("\n Tableau synthese SQ3-A - Elasticite directe par zone :\n")
elast_zone %>%
  select(Zone, eps_moy_SP95, eps_med_SP95, IC_inf, IC_sup, eps_moy_Gaz, Interpretation) %>%
  rename(
    "epsilon SP95 (moy)"   = eps_moy_SP95,
    "epsilon SP95 (med)"   = eps_med_SP95,
    "IC 95% inf"           = IC_inf,
    "IC 95% sup"           = IC_sup,
    "epsilon Gazole (moy)" = eps_moy_Gaz
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 4))) %>%
  print()

g2 <- ggplot(elast_zone, aes(x = Zone, y = eps_moy_SP95, fill = Zone)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2, linewidth = 0.8) +
  geom_hline(yintercept =  0, linetype = "solid") +
  geom_hline(yintercept = -1, linetype = "dashed", colour = "#F44336", alpha = 0.6) +
  annotate("text", x = 3.4, y = -1.05, label = "Elasticite unitaire",
           size = 3, colour = "#F44336") +
  scale_fill_manual(values = couleurs_zone) +
  labs(
    title    = "G2 - SQ3 : Elasticite-prix directe de la demande de trafic (SP95)",
    subtitle = "epsilon = Delta Trafic% / Delta Prix%  |  IC 95%  |  Rurale ~ 0 -> contrainte",
    x = NULL, y = "Elasticite (epsilon)", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# ── SQ3-B : Régression log-linéaire structurelle ─────────────────────────────
# ln(Trafic) = alpha + beta * ln(Prix_SP95) + epsilon   ->   beta = elasticite structurelle
modeles_loglin <- df_abs %>%
  group_by(Zone) %>%
  nest() %>%
  mutate(
    modele_SP95   = map(data, ~lm(log(Trafic) ~ log(Prix_SP95), data = .x)),
    modele_Gazole = map(data, ~lm(log(Trafic) ~ log(Prix_Gazole), data = .x)),
    modele_TC     = map(data, ~lm(log(Voyageurs + 1) ~ log(Prix_SP95), data = .x)),
    tidied_SP95   = map(modele_SP95,   tidy, conf.int = TRUE),
    glanced_SP95  = map(modele_SP95,   glance),
    tidied_Gaz    = map(modele_Gazole, tidy, conf.int = TRUE),
    glanced_Gaz   = map(modele_Gazole, glance),
    tidied_TC     = map(modele_TC,     tidy, conf.int = TRUE),
    glanced_TC    = map(modele_TC,     glance)
  )

res_loglin_SP95 <- modeles_loglin %>%
  unnest(tidied_SP95) %>%
  filter(term == "log(Prix_SP95)") %>%
  left_join(
    modeles_loglin %>% unnest(glanced_SP95) %>%
      select(Zone, r.squared, adj.r.squared, p.value),
    by = "Zone"
  ) %>%
  select(Zone, estimate, std.error, statistic, p.value.x,
         conf.low, conf.high, r.squared, adj.r.squared)

res_loglin_TC <- modeles_loglin %>%
  unnest(tidied_TC) %>%
  filter(term == "log(Prix_SP95)") %>%
  left_join(
    modeles_loglin %>% unnest(glanced_TC) %>% select(Zone, r.squared),
    by = "Zone"
  ) %>%
  select(Zone, estimate, std.error, p.value, conf.low, conf.high, r.squared)

cat("\n SQ3-B - Regression log-lineaire (SP95 -> Trafic) : beta = elasticite structurelle\n")
res_loglin_SP95 %>%
  mutate(
    Significatif = case_when(
      p.value.x < 0.001 ~ "***",
      p.value.x < 0.01  ~ "**",
      p.value.x < 0.05  ~ "*",
      TRUE              ~ "ns"
    ),
    Interpretation = case_when(
      abs(estimate) < 0.15 ~ "Tres inelastique",
      abs(estimate) < 0.40 ~ "Peu elastique",
      abs(estimate) < 1.00 ~ "Elastique modere",
      TRUE                 ~ "Tres elastique"
    )
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 4))) %>%
  print()

cat("\n SQ3-B - Regression log-lineaire (SP95 -> TC) : report modal structural\n")
res_loglin_TC %>% mutate(across(where(is.numeric), ~round(.x, 4))) %>% print()

g5 <- ggplot(df_abs, aes(x = log(Prix_SP95), y = log(Trafic), colour = Zone)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_colour_manual(values = couleurs_zone) +
  stat_regline_equation(
    aes(label = after_stat(eq.label)),
    size = 3, label.x.npc = "left", label.y.npc = "top"
  ) +
  labs(
    title    = "G5 - SQ3 : Regression log-lineaire : ln(Trafic) ~ ln(Prix_SP95)",
    subtitle = "Pente = beta = elasticite structurelle constante (approche Nijkamp & Gelman)",
    x = "ln(Prix SP95)", y = "ln(Trafic routier)", colour = "Zone"
  ) +
  theme_minimal(base_size = 12)


# ══════════════════════════════════════════════════════════════════════════════
#  TABLEAU DE SYNTHÈSE FINAL
# ══════════════════════════════════════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  TABLEAU DE SYNTHESE FINAL (pour le rapport)\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

corr_zone <- df_abs %>%
  group_by(Zone) %>%
  summarise(
    corr_SP95_trafic = cor(Prix_SP95, Trafic, use = "complete.obs"),
    .groups = "drop"
  )

synthese_finale <- corr_zone %>%
  left_join(elast_zone     %>% select(Zone, eps_moy_SP95, Interpretation),           by = "Zone") %>%
  left_join(cross_zone     %>% select(Zone, eps_crois_moy),                           by = "Zone") %>%
  left_join(seuil_rupture  %>% select(Zone, Faix_Rupture, Delta_max),                 by = "Zone") %>%
  left_join(res_loglin_SP95 %>% select(Zone, beta_loglin = estimate,
                                        R2 = r.squared, Sig = p.value.x),             by = "Zone") %>%
  rename(
    "Correlation (r)"    = corr_SP95_trafic,
    "epsilon directe"    = eps_moy_SP95,
    "Interpretation"     = Interpretation,
    "epsilon croisee TC" = eps_crois_moy,
    "Seuil rupture"      = Faix_Rupture,
    "Delta epsilon"      = Delta_max,
    "beta log-lin"       = beta_loglin,
    "R2"                 = R2,
    "p-value beta"       = Sig
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))

cat("\n")
print(synthese_finale)


# ══════════════════════════════════════════════════════════════════════════════
#  VISUALISATIONS — SAUVEGARDE
# ══════════════════════════════════════════════════════════════════════════════
cat("\nSauvegarde des graphiques...\n")

ggsave("G1_introduction_temporelle.png",  g1, width = 12, height = 5, dpi = 150)
ggsave("G2_SQ3_elasticite_directe.png",   g2, width = 10, height = 6, dpi = 150)
ggsave("G3_SQ1_seuil_rupture.png",        g3, width = 12, height = 6, dpi = 150)
ggsave("G4_SQ2_elasticite_croisee.png",   g4, width = 10, height = 6, dpi = 150)
ggsave("G5_SQ3_regression_loglin.png",    g5, width = 12, height = 6, dpi = 150)
ggsave("G6_SQ2_report_modal_faixas.png",  g6, width = 12, height = 6, dpi = 150)

# Grille narrative complète pour slides (ordre narratif : intro -> SQ1 -> SQ2 -> SQ3)
grille_narrative <- (g1) / (g3 | g4) / (g2 | g5)
ggsave("SYNTHESE_DATA_STORYTELLING.png", grille_narrative, width = 20, height = 18, dpi = 150)

cat("Graphiques enregistres (7 fichiers).\n")


# ══════════════════════════════════════════════════════════════════════════════
#  EXPORT CSV DÉTAILLÉS (8 fichiers)
# ══════════════════════════════════════════════════════════════════════════════
write.csv(synthese_finale,   "RESULTATS_00_synthese_finale.csv",         row.names = FALSE, fileEncoding = "UTF-8")
write.csv(seuil_table,       "RESULTATS_SQ1_elasticite_par_faixa.csv",   row.names = FALSE, fileEncoding = "UTF-8")
write.csv(seuil_rupture,     "RESULTATS_SQ1_seuil_detecte.csv",          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(cross_zone,        "RESULTATS_SQ2_elasticite_croisee.csv",     row.names = FALSE, fileEncoding = "UTF-8")
write.csv(cross_faix,        "RESULTATS_SQ2_report_par_faixa.csv",       row.names = FALSE, fileEncoding = "UTF-8")
write.csv(elast_zone,        "RESULTATS_SQ3_elasticite_directe.csv",     row.names = FALSE, fileEncoding = "UTF-8")
write.csv(res_loglin_SP95,   "RESULTATS_SQ3_loglin_SP95_trafic.csv",     row.names = FALSE, fileEncoding = "UTF-8")
write.csv(res_loglin_TC,     "RESULTATS_SQ3_loglin_SP95_TC.csv",         row.names = FALSE, fileEncoding = "UTF-8")

cat("CSVs exportes (8 fichiers).\n")
cat("\nSCRIPT EXECUTE AVEC SUCCES !\n\n")


# ══════════════════════════════════════════════════════════════════════════════
#  GUIDE D'INTERPRETATION (console) — pour la redaction du rapport
# ══════════════════════════════════════════════════════════════════════════════
cat("======================================================================\n")
cat("  GUIDE D'INTERPRETATION POUR LE RAPPORT\n")
cat("======================================================================\n")
cat("  SQ1 - Seuil de rupture : Regarder G3 + tableau SQ1.\n")
cat("        Si Delta_max est maximal entre F2 et F3, le seuil\n")
cat("        est empiriquement ~1.70 EUR (sans l'avoir suppose).\n\n")
cat("  SQ2 - Report modal : Regarder G4 + G6.\n")
cat("        epsilon_croisee > 0 -> TC et voiture sont substituts.\n")
cat("        G6 montre si ce report s'intensifie au-dessus\n")
cat("        du seuil detecte en SQ1.\n\n")
cat("  SQ3 - Facteur geographique : Comparer G2 entre zones.\n")
cat("        Rurale -> epsilon ~ 0 : contrainte, pas de choix.\n")
cat("        Urbaine -> epsilon plus negatif : alternatives dispo.\n")
cat("        G5 (log-lin) donne le beta structural pour citer\n")
cat("        dans le rapport avec p-value et R2.\n")
cat("======================================================================\n")
