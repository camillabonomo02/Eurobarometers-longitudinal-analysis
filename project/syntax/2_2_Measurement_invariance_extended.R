#' ---
#' title: Measurement invariance — Extended (4 waves, 2012-2024)
#' author: Camilla Bonomo [extends Gnambs & Appel 2019 — lavaan instead of Mplus]
#' output:
#'    html_document:
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---
#'
#' Methodological note:
#' G&A used Mplus with MplusAutomation for invariance testing.
#' This script replicates the same sequence of models (configural ->
#' metric -> scalar) using lavaan and semTools in R, with the WLSMV
#' estimator appropriate for four-category ordinal items.
#'
#' Structure:
#'   Section 1 — Measurement invariance across waves 1-3 (3 items; replicates G&A)
#'   Section 2 — Comparability across waves 1-4 (2 items; polychoric correlations)
#'   Section 3 — Practical significance of non-invariance (replicates G&A)
#'   Section 4 — Summary and methodological decision


#' **Clear workspace**
rm(list = ls())

#' **Load packages**
library(lavaan)
library(semTools)
library(polycor)     # polychoric correlations for Section 2
library(doBy)
source("./syntax/0_Start.R")

#' **Load data**
load("./data/dat.Rdata")
rm(dati_mice)

#' **Convert attitude items to ordered factors and prepare subsets**
dat$rob1 <- ordered(dat$rob1)
dat$rob2 <- ordered(dat$rob2)
dat$rob3 <- ordered(dat$rob3)

# Waves 1-3: three-item scale (replicates G&A)
d123 <- dat[dat$wave %in% 1:3, ]
d123$wave_f <- factor(d123$wave, labels = c("w2012", "w2014", "w2017"))

# Waves 1-4: two-item scale (rob3 not comparable in wave 4)
d1234 <- dat[dat$wave %in% 1:4, ]
d1234$wave_f <- factor(d1234$wave,
                       labels = c("w2012", "w2014", "w2017", "w2024"))




#' ===================================================================
#' # 1. Measurement invariance across waves 1-3 [G&A replication]
#' ===================================================================
#' Single-factor model (F) with three indicators: rob1, rob2, rob3.
#' Estimator: WLSMV; three groups (waves); sequence: configural -> metric -> scalar.

cat("\n=== MEASUREMENT INVARIANCE: WAVES 1-3 (3 items) ===\n")
cat("Replication of Gnambs & Appel (2019)\n\n")

model_3item <- '
  F =~ rob1 + rob2 + rob3
'

#' -------------------------------------------------------------------
#' ## 1a. Configural model
#' -------------------------------------------------------------------
cat("--- Model 1: Configural ---\n")
fit_conf_123 <- cfa(model_3item,
                    data        = d123,
                    group       = "wave_f",
                    estimator   = "WLSMV",
                    ordered     = c("rob1", "rob2", "rob3"),
                    group.equal = character(0))

fitMeasures(fit_conf_123, c("cfi", "tli", "rmsea", "srmr"))


#' -------------------------------------------------------------------
#' ## 1b. Metric model (loadings constrained equal across waves)
#' -------------------------------------------------------------------
cat("\n--- Model 2: Metric ---\n")
fit_metr_123 <- cfa(model_3item,
                    data        = d123,
                    group       = "wave_f",
                    estimator   = "WLSMV",
                    ordered     = c("rob1", "rob2", "rob3"),
                    group.equal = "loadings")

fitMeasures(fit_metr_123, c("cfi", "tli", "rmsea", "srmr"))


#' -------------------------------------------------------------------
#' ## 1c. Scalar model (loadings and thresholds constrained equal)
#' -------------------------------------------------------------------
cat("\n--- Model 3: Scalar ---\n")
fit_scal_123 <- cfa(model_3item,
                    data        = d123,
                    group       = "wave_f",
                    estimator   = "WLSMV",
                    ordered     = c("rob1", "rob2", "rob3"),
                    group.equal = c("loadings", "thresholds"))

fitMeasures(fit_scal_123, c("cfi", "tli", "rmsea", "srmr"))


#' -------------------------------------------------------------------
#' ## 1d. Summary table with delta fit indices
#' -------------------------------------------------------------------
cat("\n--- Model comparison (waves 1-3) ---\n")
#' Decision criteria: |dCFI| < .010 and |dRMSEA| < .015 (Cheung & Rensvold 2002).
#' With N ~ 75,000 the chi-square difference test is almost always significant;
#' incremental fit indices are therefore preferred.

cfi_123   <- sapply(list(fit_conf_123, fit_metr_123, fit_scal_123),
                    function(x) fitMeasures(x, "cfi"))
rmsea_123 <- sapply(list(fit_conf_123, fit_metr_123, fit_scal_123),
                    function(x) fitMeasures(x, "rmsea"))
tli_123   <- sapply(list(fit_conf_123, fit_metr_123, fit_scal_123),
                    function(x) fitMeasures(x, "tli"))
df_123    <- sapply(list(fit_conf_123, fit_metr_123, fit_scal_123),
                    function(x) fitMeasures(x, "df"))

tab_123 <- data.frame(
  Model  = c("Configural", "Metric", "Scalar"),
  df     = df_123,
  CFI    = round(cfi_123,   3),
  TLI    = round(tli_123,   3),
  RMSEA  = round(rmsea_123, 3),
  dCFI   = round(c(NA, diff(cfi_123)),   3),
  dRMSEA = round(c(NA, diff(rmsea_123)), 3)
)
cat("\nFit indices summary (waves 1-3, 3 items):\n")
print(tab_123)




#' ===================================================================
#' # 2. Comparability waves 1-4: polychoric correlations (2 items)
#' ===================================================================
#' With only two indicators, the configural CFA has df = 0 and is
#' not identified. The appropriate approach is to compare polychoric
#' correlations across waves (functional equivalent of metric invariance
#' testing for two-item scales) and to inspect response distributions
#' (approximate scalar invariance). See Raykov (2012), Psychological Assessment.

cat("\n\n=== COMPARABILITY WAVES 1-4: 2-ITEM SCALE (rob1 + rob2) ===\n")
cat("[EXTENSION] CFA not identified with 2 items (configural df = 0).\n")
cat("Approach: polychoric correlations + response distribution inspection.\n\n")

#' -------------------------------------------------------------------
#' ## 2a. Polychoric correlations rob1 x rob2 by wave
#' -------------------------------------------------------------------
#' Metric invariance is approximated when polychoric correlations are
#' stable across waves (criterion: dr < .05).

cat("--- Polychoric correlations rob1 x rob2 by wave ---\n")
polychor_tab <- data.frame(wave   = integer(),
                           year   = character(),
                           r_poly = numeric(),
                           SE     = numeric(),
                           N      = integer())

for (w in 1:4) {
  sub <- d1234[d1234$wave == w &
                 !is.na(d1234$rob1) & !is.na(d1234$rob2), ]
  pc  <- polychor(as.numeric(sub$rob1), as.numeric(sub$rob2),
                  std.err = TRUE)
  polychor_tab <- rbind(polychor_tab,
                        data.frame(wave   = w,
                                   year   = c("2012","2014","2017","2024")[w],
                                   r_poly = round(pc$rho, 3),
                                   SE     = round(sqrt(pc$var[1,1]), 3),
                                   N      = nrow(sub)))
}
print(polychor_tab)
cat(sprintf("\n  Max dr between adjacent waves: %.3f\n",
            max(abs(diff(polychor_tab$r_poly)))))
cat("  Criterion: dr < .05 -> metric invariance supported\n")


#' -------------------------------------------------------------------
#' ## 2b. Response distributions by wave
#' -------------------------------------------------------------------
cat("\n--- Response distributions for rob1 by wave (proportions) ---\n")
print(round(prop.table(
  table(as.numeric(d1234$rob1), d1234$wave_f), margin = 2), 3))

cat("\n--- Response distributions for rob2 by wave (proportions) ---\n")
print(round(prop.table(
  table(as.numeric(d1234$rob2), d1234$wave_f), margin = 2), 3))

cat("
  Interpretation: stable proportions (max D < .05) support approximate
  scalar invariance. Systematic differences indicate shifts in response
  thresholds across waves (response style change), which should be
  acknowledged as a limitation if observed.
")




#' ===================================================================
#' # 3. Practical significance of non-invariance [G&A replication]
#' ===================================================================
#' G&A verify that differences in predicted response probabilities
#' between the configural and scalar models do not exceed .06.
#' Uses ThresholdProbability() defined in 0_Start.R.

cat("\n\n=== PRACTICAL SIGNIFICANCE OF NON-INVARIANCE (waves 1-3) ===\n")
cat("Replication of Gnambs & Appel (2019)\n\n")

#' Helper: extract model parameters from a lavaan object for a given group
extract_params_lavaan <- function(fit, group_idx, items) {

  params <- parameterestimates(fit, standardized = FALSE)

  # Factor mean (fixed to 0 in the reference group)
  F_mean <- params$est[params$lhs == "F" &
                         params$op  == "~1" &
                         params$group == group_idx]
  if (length(F_mean) == 0) F_mean <- 0

  # Factor variance
  F_var <- params$est[params$lhs == "F" &
                        params$op  == "~~" &
                        params$rhs == "F" &
                        params$group == group_idx]
  if (length(F_var) == 0) F_var <- 1

  # Factor loadings
  loadings <- sapply(items, function(it) {
    v <- params$est[params$lhs == "F" &
                      params$op  == "=~" &
                      params$rhs == it &
                      params$group == group_idx]
    if (length(v) == 0) NA else v[1]
  })

  # Thresholds (3 thresholds per 4-category item)
  thresholds <- lapply(items, function(it) {
    th <- params$est[params$lhs == it &
                       params$op  == "|" &
                       params$group == group_idx]
    if (length(th) < 3) rep(NA, 3) else th[1:3]
  })
  th_mat <- do.call(rbind, thresholds)

  list(F_mean     = F_mean,
       F_var      = F_var,
       loadings   = loadings,
       thresholds = th_mat)
}

items_3 <- c("rob1", "rob2", "rob3")

for (g in 1:3) {
  wave_label <- c("2012", "2014", "2017")[g]
  cat(sprintf("Wave %s:\n", wave_label))

  p_conf <- tryCatch({
    pr <- extract_params_lavaan(fit_conf_123, g, items_3)
    ThresholdProbability(3, pr$loadings, pr$F_mean, pr$F_var,
                         rep(1, 3), 3, pr$thresholds)
  }, error = function(e) {
    cat("  [Error extracting configural parameters]\n"); NULL
  })

  p_scal <- tryCatch({
    pr <- extract_params_lavaan(fit_scal_123, g, items_3)
    ThresholdProbability(3, pr$loadings, pr$F_mean, pr$F_var,
                         rep(1, 3), 3, pr$thresholds)
  }, error = function(e) {
    cat("  [Error extracting scalar parameters]\n"); NULL
  })

  if (!is.null(p_conf) && !is.null(p_scal)) {
    diff_mat <- round(p_conf - p_scal, 3)
    rownames(diff_mat) <- items_3
    cat("  Probability differences (Configural - Scalar):\n")
    print(diff_mat)
    max_diff <- max(abs(diff_mat), na.rm = TRUE)
    cat(sprintf("  Max absolute difference: %.3f  %s\n\n",
                max_diff,
                ifelse(max_diff < .06,
                       "OK: < .06 (non-invariance not practically significant)",
                       "ATTENTION: > .06 (discuss as limitation)")))
  }
}




#' ===================================================================
#' # 4. Summary and methodological decision
#' ===================================================================

cat("\n=== FINAL SUMMARY ===\n\n")

cat("--- Waves 1-3 (3 items, G&A replication) ---\n")
print(tab_123)

cat("\n--- Waves 1-4 (2 items, extension) ---\n")
print(polychor_tab)

cat("
=== DECISION CRITERIA ===

Waves 1-3 (CFA-based):
  |dCFI|   < .010 -> invariance supported  (Cheung & Rensvold 2002)
  |dRMSEA| < .015 -> invariance supported
  Max Dprob < .06  -> non-invariance not practically significant (G&A)

Waves 1-4 (polychoric correlation-based):
  dr_poly  < .05  -> approximate metric invariance supported
  Stable proportions -> approximate scalar invariance

=== DECISION FOR THE MAIN ANALYSIS ===

  If (at minimum) metric invariance is supported:
    -> Use the three-item composite (rob) for the waves 1-3 multilevel analysis
    -> Use the two-item composite (rob2item) for all comparisons extending to wave 4
    -> Document both choices in the methodological appendix (Ch. 4.3.4)

  If scalar invariance is not supported (dCFI > .010):
    -> Correlation and regression comparisons remain legitimate
    -> Mean comparisons require caution: report as a methodological limitation
    -> G&A proceed under approximate invariance; the same decision applies here
")

cat("\nScript 2_2 complete. Proceed to 3_Current_and_changes_extended.R\n")
