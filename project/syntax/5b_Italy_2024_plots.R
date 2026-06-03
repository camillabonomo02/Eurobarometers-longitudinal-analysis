#' ---
#' title: Italy 2024 — SP554 focus figures [EXTENSION]
#' author: Camilla Bonomo
#' output:
#'    html_document:
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---
#'
#' Figures accompanying Script 4b_Italy_2024_focus.R.
#' Numbering continues from 5_Plots_extended.R (last figure = 16).
#'
#' Figures produced:
#'   Figure 17 — Distribution of ai_exposure_2024: Italy vs EU27
#'   Figure 18 — rob2024_pos and rob2024_neg by exposure level (Italy)
#'   Figure 19 — Correlation heatmap: 5 indicators x 2 subscales (Italy)
#'   Figure 20 — Mediation forest plot: digital_self_efficacy_2024 (Italy)
#'
#' Colour conventions (same as 5_Plots_extended.R):
#'   col_italy   = #D55E00 (Okabe-Ito vermilion)
#'   col_eu      = #0072B2 (Okabe-Ito blue)
#'   col_neutral = #999999 (grey)
#'   pal_div     = diverging red-white-blue (for heatmap)
#' All blocks marked [EXTENSION].


#' **Clear workspace**
rm(list = ls())

#' **Load packages**
library(haven)
library(ggplot2)
library(ggrepel)
library(lme4)
library(lmerTest)
library(mitml)
library(weights)
library(doBy)
source("./syntax/0_Start.R")

#' **Country codes**
isocntry <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GR",
              "HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO",
              "SE","SI","SK")
cid_seq <- seq_len(length(isocntry))

#' **Load data**
load("./data/dat.Rdata")
rm(dati_mice)

#' **Recode dati (same as Scripts 4 and 4b)**
dati <- within(dati, {
  white <- as.factor(white)
  sex   <- as.factor(sex)
  wave  <- as.factor(wave)
  age   <- scale(age,  scale = FALSE) / 10
  educ  <- scale(educ, scale = FALSE)
})
dati <- lapply(dati, function(x) {
  x$rob2item    <- x$rob1 + x$rob2
  if ("r24_c" %in% names(x)) {
    x$rob2024_pos <- ifelse(x$wave == 4, x$rob2 + x$rob3,    NA_real_) # [EXTENSION]
    x$rob2024_neg <- ifelse(x$wave == 4, x$r24_c + x$r24_d, NA_real_) # [EXTENSION]
  }
  x
})
dati <- as.mitml.list(dati)

dir.create("./plots", showWarnings = FALSE)




#' ===================================================================
#' # GLOBAL AESTHETICS (replicated from 5_Plots_extended.R)
#' ===================================================================

pal_div <- colorRampPalette(c("#b2182b","#ef8a62","#fddbc7",
                               "#f7f7f7",
                               "#d1e5f0","#67a9cf","#2166ac"))
col_italy   <- "#D55E00"
col_eu      <- "#0072B2"
col_neutral <- "#999999"

theme_academic <- function(base_size = 12) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      panel.grid.minor    = element_blank(),
      panel.grid.major    = element_line(colour = "grey88", linewidth = 0.3),
      strip.background    = element_rect(fill = "grey94", colour = "grey70",
                                         linewidth = 0.5),
      strip.text          = element_text(size = base_size, face = "bold",
                                         margin = margin(4, 4, 4, 4)),
      legend.background   = element_blank(),
      legend.key          = element_blank(),
      legend.title        = element_text(size = base_size - 1, face = "bold"),
      legend.text         = element_text(size = base_size - 1),
      plot.title          = element_text(size = base_size + 1, face = "bold",
                                         hjust = 0, margin = margin(b = 3)),
      plot.subtitle       = element_text(size = base_size - 1, colour = "grey40",
                                         hjust = 0, margin = margin(b = 5)),
      plot.caption        = element_text(size = base_size - 2, colour = "grey50",
                                         hjust = 0, margin = margin(t = 4)),
      axis.text           = element_text(colour = "grey30", size = base_size - 1),
      axis.title          = element_text(size = base_size, face = "bold"),
      plot.margin         = margin(10, 12, 10, 10)
    )
}




#' ===================================================================
#' # BUILD INDICATORS (self-contained rebuild from ZA8844)
#' ===================================================================

sp554_raw <- read_dta("./rawdata/ZA8844_v1-0-0.dta")          # [EXTENSION]
sp554_raw$cntry <- recodeVar(trimws(sp554_raw$isocntry),       # [EXTENSION]
                              c("DE-E","DE-W"), c("DE","DE"))  # [EXTENSION]
sp554_raw$cid   <- as.numeric(recodeVar(                       # [EXTENSION]
  sp554_raw$cntry, isocntry, cid_seq, default = NA))           # [EXTENSION]
sp554 <- sp554_raw[!is.na(sp554_raw$cid), ]                    # [EXTENSION]
rm(sp554_raw)                                                   # [EXTENSION]

it_mask <- sp554$cntry == "IT"

# --- A: ai_exposure_2024 ---
ai_bin <- sapply(paste0("qb7_", 1:6), function(v) {            # [EXTENSION]
  x <- as.numeric(zap_labels(sp554[[v]]))                      # [EXTENSION]
  ifelse(x %in% 1:2, 1L, ifelse(x %in% 3:4, 0L, NA_integer_))# [EXTENSION]
})                                                              # [EXTENSION]
ai_exposure_2024 <- rowSums(ai_bin, na.rm = TRUE)              # [EXTENSION]
ai_exposure_2024[is.na(zap_labels(sp554$qb7_1))] <- NA        # [EXTENSION]
ai_exposure_high <- ifelse(!is.na(ai_exposure_2024),           # [EXTENSION]
                            as.integer(ai_exposure_2024 >= 3), # [EXTENSION]
                            NA_integer_)                        # [EXTENSION]

# --- B: regulation_demand_2024 ---
b_mat <- sapply(paste0("qb11_", 1:5), function(v) {            # [EXTENSION]
  x <- as.numeric(zap_labels(sp554[[v]]))                      # [EXTENSION]
  x[x == 5] <- NA; 4L - x                                      # [EXTENSION]
})                                                              # [EXTENSION]
regulation_demand_2024 <- rowMeans(b_mat, na.rm = TRUE)        # [EXTENSION]
regulation_demand_2024[rowSums(!is.na(b_mat)) == 0] <- NA      # [EXTENSION]

# --- C: digital_self_efficacy_2024 ---
c_mat <- sapply(c("qb2_1","qb2_4"), function(v) {              # [EXTENSION]
  x <- as.numeric(zap_labels(sp554[[v]]))                      # [EXTENSION]
  x[x %in% 5:6] <- NA; 4L - x                                  # [EXTENSION]
})                                                              # [EXTENSION]
digital_self_efficacy_2024 <- rowMeans(c_mat, na.rm = TRUE)    # [EXTENSION]
digital_self_efficacy_2024[rowSums(!is.na(c_mat)) == 0] <- NA  # [EXTENSION]

# --- D: tech_pessimism_2024 ---
d_mat <- sapply(paste0("qb1_", 1:4), function(v) {             # [EXTENSION]
  x <- as.numeric(zap_labels(sp554[[v]]))                      # [EXTENSION]
  x[x %in% 5:7] <- NA; x - 1L                                  # [EXTENSION]
})                                                              # [EXTENSION]
tech_pessimism_2024 <- rowMeans(d_mat, na.rm = TRUE)           # [EXTENSION]
tech_pessimism_2024[rowSums(!is.na(d_mat)) == 0] <- NA         # [EXTENSION]

# --- E: employer_communication_2024 ---
e_any <- as.integer(                                            # [EXTENSION]
  as.numeric(zap_labels(sp554$qb9_1)) == 1 |                  # [EXTENSION]
  as.numeric(zap_labels(sp554$qb9_2)) == 1 |                  # [EXTENSION]
  as.numeric(zap_labels(sp554$qb9_3)) == 1 |                  # [EXTENSION]
  as.numeric(zap_labels(sp554$qb9_4)) == 1)                   # [EXTENSION]
employer_communication_2024 <- ifelse(                          # [EXTENSION]
  is.na(zap_labels(sp554$qb9_1)), NA_real_,                   # [EXTENSION]
  ifelse(as.numeric(zap_labels(sp554$qb9_6)) == 1 |           # [EXTENSION]
         as.numeric(zap_labels(sp554$qb9_7)) == 1, NA_real_,  # [EXTENSION]
  e_any))                                                       # [EXTENSION]
rm(e_any, ai_bin, b_mat, c_mat, d_mat)                         # [EXTENSION]

# --- Merge into dati and raw dat_w4 ---
pid_w4 <- dat$pid[dat$wave == 4]
ind_lookup <- data.frame(                                       # [EXTENSION]
  pid                         = pid_w4,                        # [EXTENSION]
  ai_exposure_2024            = ai_exposure_2024,              # [EXTENSION]
  ai_exposure_high            = ai_exposure_high,              # [EXTENSION]
  regulation_demand_2024      = regulation_demand_2024,        # [EXTENSION]
  digital_self_efficacy_2024  = digital_self_efficacy_2024,    # [EXTENSION]
  tech_pessimism_2024         = tech_pessimism_2024,           # [EXTENSION]
  employer_communication_2024 = employer_communication_2024,   # [EXTENSION]
  stringsAsFactors = FALSE)                                     # [EXTENSION]

dati <- lapply(dati, function(x) merge(x, ind_lookup, by="pid", all.x=TRUE))
dati <- as.mitml.list(dati)

dat_w4 <- dat[dat$wave == 4, ]
for (v in names(ind_lookup)[-1]) dat_w4[[v]] <- ind_lookup[[v]]
dat_w4$rob2024_pos <- dat_w4$rob2 + dat_w4$rob3
dat_w4$rob2024_neg <- dat_w4$r24_c + dat_w4$r24_d

# Weighted mean/SD helper
wmsd <- function(x, w) {                                       # [EXTENSION]
  ok <- !is.na(x) & !is.na(w)                                 # [EXTENSION]
  m  <- weighted.mean(x[ok], w[ok])                           # [EXTENSION]
  v  <- sum(w[ok] * (x[ok] - m)^2) / sum(w[ok])              # [EXTENSION]
  se <- sqrt(v / sum(ok))                                      # [EXTENSION]
  c(mean = m, sd = sqrt(v), se = se, n = sum(ok))             # [EXTENSION]
}                                                              # [EXTENSION]




#' ===================================================================
#' # 17. Distribution of ai_exposure_2024: Italy vs EU27 [EXTENSION]
#' ===================================================================
#' Weighted proportions at each count value (0-6).
#' Italy in col_italy, EU27 in col_eu.

cat("\n=== FIGURE 17: ai_exposure_2024 DISTRIBUTION ===\n")

prop_tbl <- do.call(rbind, lapply(c("Italy","EU27"), function(grp) { # [EXTENSION]
  sub <- if (grp == "Italy") dat_w4[dat_w4$cntry == "IT", ]         # [EXTENSION]
         else dat_w4                                                  # [EXTENSION]
  sub <- sub[!is.na(sub$ai_exposure_2024) & !is.na(sub$wgt2), ]     # [EXTENSION]
  counts <- 0:6                                                       # [EXTENSION]
  props  <- sapply(counts, function(k)                               # [EXTENSION]
    sum(sub$wgt2[sub$ai_exposure_2024 == k]) / sum(sub$wgt2))        # [EXTENSION]
  data.frame(group = grp, count = counts, prop = props,              # [EXTENSION]
             mean  = weighted.mean(sub$ai_exposure_2024, sub$wgt2))  # [EXTENSION]
}))                                                                    # [EXTENSION]

p17 <- ggplot(prop_tbl, aes(x = factor(count), y = prop,            # [EXTENSION]
                              fill = group, colour = group)) +        # [EXTENSION]
  geom_col(position = position_dodge(0.85), width = 0.78,            # [EXTENSION]
           alpha = 0.82, colour = "white") +                          # [EXTENSION]
  scale_fill_manual(values = c(EU27 = col_eu, Italy = col_italy),     # [EXTENSION]
                    name = NULL) +                                     # [EXTENSION]
  scale_colour_manual(values = c(EU27 = col_eu, Italy = col_italy),   # [EXTENSION]
                      name = NULL) +                                   # [EXTENSION]
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),   # [EXTENSION]
                     limits = c(0, 0.45),                              # [EXTENSION]
                     expand = expansion(mult = c(0, 0.04))) +          # [EXTENSION]
  scale_x_discrete(labels = as.character(0:6)) +                      # [EXTENSION]
  labs(title    = "AI/digital tech exposure in the workplace: Italy vs EU27 (wave 4, 2024)",
       subtitle = "Weighted proportion of respondents reporting N workplace AI activities (0-6).",
       x        = "Number of AI activities reported (ai_exposure_2024)",
       y        = "Proportion of respondents",
       caption  = "QB7_1-6: hiring, task allocation, scheduling, monitoring, performance assessment, safety. Weighted.") +
  theme_academic(base_size = 12) +                                     # [EXTENSION]
  theme(legend.position    = "bottom",                                 # [EXTENSION]
        panel.grid.major.x = element_blank())                          # [EXTENSION]

ggsave("./plots/Figure_17_ai_exposure_distribution.png", p17,         # [EXTENSION]
       width = 11, height = 6, dpi = 150)                             # [EXTENSION]
cat("Saved: Figure_17_ai_exposure_distribution.png\n")
rm(prop_tbl, mean_lbl, p17)




#' ===================================================================
#' # 18. rob2024_pos and rob2024_neg by exposure level (Italy) [EXTENSION]
#' ===================================================================
#' Bar chart: Low exposure (<=1) vs High exposure (>=3).
#' Two bars per group: rob2024_pos (utility) and rob2024_neg (threat-absence).
#' Error bars = +/- 1 SE. Labels above bars.

cat("\n=== FIGURE 18: ASYMMETRY BY EXPOSURE LEVEL (Italy) ===\n")

it_strat <- dat_w4[dat_w4$cntry == "IT" &
                   !is.na(dat_w4$ai_exposure_high) &
                   dat_w4$ai_exposure_high %in% c(0, 1), ]           # [EXTENSION]

bar_data <- do.call(rbind, lapply(c(0, 1), function(grp) {            # [EXTENSION]
  g    <- it_strat[it_strat$ai_exposure_high == grp, ]                # [EXTENSION]
  lbl  <- if (grp == 1) "High (>=3)" else "Low (<=1)"                 # [EXTENSION]
  rbind(                                                               # [EXTENSION]
    { s <- wmsd(g$rob2024_pos, g$wgt2)                                # [EXTENSION]
      data.frame(exposure = lbl, subscale = "Utility\n(rob2024_pos)", # [EXTENSION]
                 mean = s["mean"], se = s["se"], n = s["n"]) },       # [EXTENSION]
    { s <- wmsd(g$rob2024_neg, g$wgt2)                                # [EXTENSION]
      data.frame(exposure = lbl, subscale = "Threat-absence\n(rob2024_neg)",# [EXTENSION]
                 mean = s["mean"], se = s["se"], n = s["n"]) }        # [EXTENSION]
  )                                                                    # [EXTENSION]
}))                                                                    # [EXTENSION]
bar_data$exposure <- factor(bar_data$exposure,
                             levels = c("Low (<=1)","High (>=3)"))
bar_data$subscale <- factor(bar_data$subscale,
                             levels = c("Utility\n(rob2024_pos)",
                                        "Threat-absence\n(rob2024_neg)"))

col_subscale <- c("Utility\n(rob2024_pos)"      = col_eu,
                  "Threat-absence\n(rob2024_neg)" = col_italy)

p18 <- ggplot(bar_data, aes(x = exposure, y = mean,                   # [EXTENSION]
                              fill = subscale, group = subscale)) +    # [EXTENSION]
  geom_col(position = position_dodge(0.72), width = 0.64,             # [EXTENSION]
           alpha = 0.85, colour = "white") +                           # [EXTENSION]
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),              # [EXTENSION]
                position = position_dodge(0.72), width = 0.22,        # [EXTENSION]
                linewidth = 0.7, colour = "grey30") +                  # [EXTENSION]
  geom_text(aes(y = mean + se + 0.06,                                  # [EXTENSION]
                label = sprintf("%.2f", mean)),                        # [EXTENSION]
            position = position_dodge(0.72),                           # [EXTENSION]
            size = 3.4, fontface = "bold", colour = "grey20") +       # [EXTENSION]
  scale_fill_manual(values = col_subscale, name = "Subscale") +       # [EXTENSION]
  scale_y_continuous(limits = c(0, 3.5),                              # [EXTENSION]
                     breaks = seq(0, 3, 0.5),                          # [EXTENSION]
                     expand = expansion(mult = c(0, 0.05))) +          # [EXTENSION]
  labs(title    = "Robot attitude subscales by AI workplace exposure — Italy 2024",
       subtitle = "Low = ≤1 AI activities reported; High = ≥3. Error bars: ±1 SE. Weighted means.",
       x        = "AI exposure level (ai_exposure_high)",
       y        = "Mean subscale score (0-6 rescaled to 0-3)",
       caption  = "rob2024_pos: social utility of automation; rob2024_neg: low perceived job-loss threat.\nMedium exposure (=2) excluded for clarity.") +
  theme_academic(base_size = 12) +                                     # [EXTENSION]
  theme(legend.position   = "bottom",                                  # [EXTENSION]
        panel.grid.major.x = element_blank())                          # [EXTENSION]

ggsave("./plots/Figure_18_subscales_by_exposure.png", p18,            # [EXTENSION]
       width = 10, height = 7, dpi = 150)                             # [EXTENSION]
cat("Saved: Figure_18_subscales_by_exposure.png\n")
rm(it_strat, bar_data, col_subscale, p18)




#' ===================================================================
#' # 19. Correlation heatmap: 5 indicators x 2 subscales (Italy) [EXTENSION]
#' ===================================================================
#' Weighted Pearson r on Italian wave-4 observed data (first imp. dataset).
#' The official pooled r values are in Script 4b Section 5.

cat("\n=== FIGURE 19: CORRELATION HEATMAP (Italy) ===\n")

d1_it <- dati[[1]][dati[[1]]$wave == 4 & dati[[1]]$cntry == "IT", ]  # [EXTENSION]

ind_vars    <- c("ai_exposure_2024","regulation_demand_2024",
                 "digital_self_efficacy_2024","tech_pessimism_2024",
                 "employer_communication_2024")
ind_labels  <- c("AI exposure\n(A)",
                 "Regulation demand\n(B)",
                 "Digital self-efficacy\n(C)",
                 "Tech pessimism\n(D)",
                 "Employer\ncommunication (E)")
out_vars    <- c("rob2024_pos","rob2024_neg")
out_labels  <- c("Utility\n(rob2024_pos)","Threat-absence\n(rob2024_neg)")

heat_df <- do.call(rbind, lapply(seq_along(ind_vars), function(i) {   # [EXTENSION]
  do.call(rbind, lapply(seq_along(out_vars), function(j) {            # [EXTENSION]
    x   <- d1_it[[ind_vars[i]]]                                       # [EXTENSION]
    y   <- d1_it[[out_vars[j]]]                                       # [EXTENSION]
    w   <- d1_it$wgt2                                                  # [EXTENSION]
    ok  <- !is.na(x) & !is.na(y) & !is.na(w)                        # [EXTENSION]
    r   <- if (sum(ok) > 10) {                                        # [EXTENSION]
             wt_x <- x[ok] - weighted.mean(x[ok], w[ok])             # [EXTENSION]
             wt_y <- y[ok] - weighted.mean(y[ok], w[ok])             # [EXTENSION]
             sum(w[ok] * wt_x * wt_y) /                              # [EXTENSION]
               sqrt(sum(w[ok] * wt_x^2) * sum(w[ok] * wt_y^2))      # [EXTENSION]
           } else NA_real_                                             # [EXTENSION]
    data.frame(indicator = ind_labels[i], outcome = out_labels[j],   # [EXTENSION]
               r = round(r, 3), stringsAsFactors = FALSE)            # [EXTENSION]
  }))                                                                  # [EXTENSION]
}))                                                                    # [EXTENSION]

heat_df$indicator <- factor(heat_df$indicator, levels = rev(ind_labels))
heat_df$outcome   <- factor(heat_df$outcome,   levels = out_labels)

p19 <- ggplot(heat_df, aes(x = outcome, y = indicator, fill = r)) +  # [EXTENSION]
  geom_tile(colour = "white", linewidth = 0.8) +                      # [EXTENSION]
  geom_text(aes(label = sprintf("%.3f", r),                           # [EXTENSION]
                colour = abs(r) > 0.25), size = 4.2,                  # [EXTENSION]
            fontface = "bold") +                                       # [EXTENSION]
  scale_fill_gradientn(colours = pal_div(11), limits = c(-1, 1),      # [EXTENSION]
                       breaks = c(-0.5, 0, 0.5), name = "r") +       # [EXTENSION]
  scale_colour_manual(values = c("TRUE" = "white","FALSE" = "grey20"),# [EXTENSION]
                      guide = "none") +                                # [EXTENSION]
  scale_x_discrete(position = "top") +                                 # [EXTENSION]
  labs(title    = "Correlations: SP554 indicators x robot attitude subscales (Italy, wave 4)",
       subtitle = "Weighted Pearson r, Italian subsample (N~788-1037). Official pooled r in Script 4b.",
       x = NULL, y = NULL,
       caption  = "A=ai_exposure  B=regulation_demand  C=digital_self_efficacy  D=tech_pessimism  E=employer_communication") +
  theme_academic(base_size = 12) +                                     # [EXTENSION]
  theme(axis.text.x       = element_text(size = 10, face = "bold"),   # [EXTENSION]
        axis.text.y       = element_text(size = 10),                  # [EXTENSION]
        panel.grid        = element_blank(),                           # [EXTENSION]
        legend.key.height = unit(1.2, "cm"))                          # [EXTENSION]

ggsave("./plots/Figure_19_correlation_heatmap_italy.png", p19,        # [EXTENSION]
       width = 9, height = 7, dpi = 150)                              # [EXTENSION]
cat("Saved: Figure_19_correlation_heatmap_italy.png\n")
rm(d1_it, heat_df, p19)




#' ===================================================================
#' # 20. Mediation forest plot: digital_self_efficacy_2024 (Italy) [EXTENSION]
#' ===================================================================
#' Two panels: rob2024_pos (left), rob2024_neg (right).
#' Per panel, key predictors (age, educ, white2, white3) shown in two rows:
#'   - Base model (without self-efficacy)
#'   - + digital_self_efficacy_2024
#' Shows shrinkage of age and educ coefficients when self-efficacy is added.

cat("\n=== FIGURE 20: MEDIATION FOREST PLOT ===\n")
cat("  Re-running pool_lm_est for all 4 models (takes ~30 sec)...\n")

it_subset <- "wave == 4 & cntry == 'IT'"

pool_lm_est <- function(dati_list, formula_str, subset_expr,   # [EXTENSION]
                        weight_var = "wgt2") {                 # [EXTENSION]
  results <- lapply(dati_list, function(x) {                   # [EXTENSION]
    sub <- x[eval(parse(text = subset_expr), envir = x), ]    # [EXTENSION]
    sub <- sub[complete.cases(                                  # [EXTENSION]
      sub[, all.vars(as.formula(formula_str))]), ]             # [EXTENSION]
    fit <- lm(as.formula(formula_str), data = sub,             # [EXTENSION]
              weights = sub[[weight_var]])                      # [EXTENSION]
    list(coef = coef(fit), vcov = diag(vcov(fit)), n = nrow(sub))# [EXTENSION]
  })                                                            # [EXTENSION]
  qhat <- sapply(results, `[[`, "coef")                        # [EXTENSION]
  uhat <- sapply(results, `[[`, "vcov")                        # [EXTENSION]
  testEstimates(qhat = qhat, uhat = uhat)                      # [EXTENSION]
}                                                              # [EXTENSION]

# Key predictors to show in the forest plot
key_preds <- c("age","educ","white2","white3")
pred_labels <- c(age = "Age (10 yr)", educ = "Education",
                 white2 = "Blue-collar", white3 = "Non-employed")

# Run the four models
e_base_pos <- pool_lm_est(dati, "rob2024_pos ~ sex + age + educ + white", it_subset)
e_med_pos  <- pool_lm_est(dati,
  "rob2024_pos ~ sex + age + educ + white + digital_self_efficacy_2024", it_subset)
e_base_neg <- pool_lm_est(dati, "rob2024_neg ~ sex + age + educ + white", it_subset)
e_med_neg  <- pool_lm_est(dati,
  "rob2024_neg ~ sex + age + educ + white + digital_self_efficacy_2024", it_subset)

cat("  Models estimated.\n")

# Assemble forest data
extract_ests <- function(est_obj, model_lbl, outcome_lbl, preds) {    # [EXTENSION]
  m <- est_obj$estimates                                               # [EXTENSION]
  do.call(rbind, lapply(preds, function(p) {                          # [EXTENSION]
    if (!(p %in% rownames(m))) return(NULL)                           # [EXTENSION]
    data.frame(pred    = pred_labels[p],                              # [EXTENSION]
               term    = p,                                           # [EXTENSION]
               model   = model_lbl,                                   # [EXTENSION]
               outcome = outcome_lbl,                                 # [EXTENSION]
               est     = m[p, "Estimate"],                            # [EXTENSION]
               se      = m[p, "Std.Error"],                           # [EXTENSION]
               stringsAsFactors = FALSE)                              # [EXTENSION]
  }))                                                                  # [EXTENSION]
}                                                                      # [EXTENSION]

fp_df <- rbind(                                                        # [EXTENSION]
  extract_ests(e_base_pos, "Base model",       "rob2024_pos\n(Utility)",        key_preds),
  extract_ests(e_med_pos,  "+ Self-efficacy",  "rob2024_pos\n(Utility)",        key_preds),
  extract_ests(e_base_neg, "Base model",       "rob2024_neg\n(Threat-absence)", key_preds),
  extract_ests(e_med_neg,  "+ Self-efficacy",  "rob2024_neg\n(Threat-absence)", key_preds)
)                                                                      # [EXTENSION]
fp_df$pred  <- factor(fp_df$pred, levels = rev(pred_labels))
fp_df$model <- factor(fp_df$model, levels = c("Base model","+ Self-efficacy"))

col_model <- c("Base model"      = col_neutral,
               "+ Self-efficacy" = col_italy)

p20 <- ggplot(fp_df, aes(x = est, y = pred,                           # [EXTENSION]
                          colour = model, shape = model)) +            # [EXTENSION]
  geom_vline(xintercept = 0, linetype = "dashed",                     # [EXTENSION]
             colour = "grey50", linewidth = 0.6) +                    # [EXTENSION]
  geom_errorbarh(aes(xmin = est - 1.96*se, xmax = est + 1.96*se),    # [EXTENSION]
                 height = 0.25, linewidth = 0.8,                      # [EXTENSION]
                 position = position_dodge(0.55)) +                   # [EXTENSION]
  geom_point(size = 3.2,                                              # [EXTENSION]
             position = position_dodge(0.55)) +                       # [EXTENSION]
  scale_colour_manual(values = col_model, name = "Model") +           # [EXTENSION]
  scale_shape_manual(values = c("Base model" = 16, "+ Self-efficacy" = 17),
                     name = "Model") +                                 # [EXTENSION]
  facet_wrap(~ outcome, ncol = 2) +                                   # [EXTENSION]
  labs(title    = "Mediation by digital self-efficacy: coefficient change for age and education (Italy)",
       subtitle = "Orange = model including digital_self_efficacy_2024. Shrinkage of age/educ = mediation.",
       x        = "Unstandardised regression coefficient (raw units)",
       y        = NULL,
       caption  = "Italy wave-4 subsample. pool_lm, Rubin pooling, m=20. Error bars: 95% CI.") +
  theme_academic(base_size = 12) +                                     # [EXTENSION]
  theme(legend.position    = "bottom",                                 # [EXTENSION]
        panel.grid.major.y = element_blank())                          # [EXTENSION]

ggsave("./plots/Figure_20_mediation_forest.png", p20,                 # [EXTENSION]
       width = 12, height = 7, dpi = 150)                             # [EXTENSION]
cat("Saved: Figure_20_mediation_forest.png\n")
rm(e_base_pos, e_med_pos, e_base_neg, e_med_neg,
   fp_df, key_preds, pred_labels, col_model, p20,
   pool_lm_est, it_subset)


cat("\n=== ALL FIGURES (5b) COMPLETE ===\n")
cat("Saved in ./plots/:\n")
cat("  Figure_17_ai_exposure_distribution.png\n")
cat("  Figure_18_subscales_by_exposure.png\n")
cat("  Figure_19_correlation_heatmap_italy.png\n")
cat("  Figure_20_mediation_forest.png\n")
cat("\nScript 5b complete.\n")
