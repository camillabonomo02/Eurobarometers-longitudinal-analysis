#' ---
#' title: Predictors of attitudes — Extended (4 waves, 2012-2024)
#' author: Camilla Bonomo [extends Gnambs & Appel 2019]
#' output:
#'    html_document:
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---
#'
#' Structure:
#'   Section 1 — Variable recoding
#'   Section 2 — Model A0: null model (ICC) by wave [G&A + extension]
#'   Section 3 — Model A1: individual-level predictors [G&A + wave 4]
#'   Section 4 — Model A2: individual + contextual predictors [G&A replication]
#'   Section 5 — Model B2: A2 + UAI [EXTENSION — H1 and H2]
#'   Section 6 — A2 vs. B2 comparison and Italian residuals
#'   Section 7 — Feel items (G&A replication, waves 1-3)
#'   Section 8 — Italy focus, wave 4: within-country regression
#'
#' Hypotheses tested:
#'   H1 (multidimensional cultural hypothesis):
#'       National cultural orientations jointly predict robot acceptance,
#'       net of the structural L2 predictors from Model A2. Specifically:
#'         H1a: UAI_z < 0  — uncertainty avoidance -> negative attitudes
#'         H1b: IDV_z > 0  — individualism -> positive attitudes
#'         H1c: LTO_z > 0  — long-term orientation -> positive attitudes
#'         H1d: PDI_z      — exploratory (power distance, sign unclear)
#'         H1e: MAS_z      — exploratory (performance vs. quality-of-life)
#'         H1f: IVR_z > 0  — indulgence -> openness to new technology
#'       H1a is tested in isolation in Model B2 (UAI only).
#'       The full hypothesis is tested in Model B2_multi (all six dimensions).
#'       Note: with N = 27 L2 units, the full model has limited df at L2;
#'       results should be interpreted as exploratory.
#'   H2 (mediation): the effect of latitude is attenuated when UAI
#'       is added to the model, indicating that UAI partially mediates
#'       the geographic North-South gradient.
#'
#' Dependent variable note:
#'   Waves 1-3 analyses use rob (three-item composite, range 0-9).
#'   Waves 1-4 analyses use rob2item (two-item comparable composite,
#'   range 0-6), because rob3 wording changed in wave 4 ("boring/
#'   repetitive" vs. "hard/dangerous"), rendering rob non-comparable
#'   across all four waves.


#' **Clear workspace**
rm(list = ls())

#' **Load packages**
library(lme4)
library(lmerTest)   # Satterthwaite p-values for lmer
library(mitml)
library(weights)
source("./syntax/0_Start.R")

#' **Load data**
load("./data/dat.Rdata")
rm(dati_mice)




#' ===================================================================
#' # 1. Variable recoding [G&A + EXTENSION wave 4]
#' ===================================================================

dati <- within(dati, {
  white <- as.factor(white)    # 1 = white-collar, 2 = blue-collar, 3 = non-employed
  sex   <- as.factor(sex)      # 0 = male, 1 = female
  wave  <- as.factor(wave)     # wave 1 as reference category
  age   <- scale(age,  scale = FALSE) / 10   # centred, unit = 10 years
  educ  <- scale(educ, scale = FALSE)         # centred
})

#' **Ensure rob2item is available** [EXTENSION]
#' rob2item is available as a passive-imputed variable in dati;
#' this step recomputes it from components as a consistency safeguard.
dati <- lapply(dati, function(x) {
  x$rob2item <- x$rob1 + x$rob2
  x
})
dati <- as.mitml.list(dati)

#' **Standardise UAI at the country level** [EXTENSION]
#' z-score computed over the 27 unique country values (not over all
#' individual observations), appropriate for a time-invariant L2 variable.
uai_mean <- mean(unique(dati[[1]][, c("cid", "UAI")])$UAI, na.rm = TRUE)
uai_sd   <- sd(unique(dati[[1]][, c("cid", "UAI")])$UAI,   na.rm = TRUE)
dati <- lapply(dati, function(x) {
  x$UAI_z <- (x$UAI - uai_mean) / uai_sd
  x
})
dati <- as.mitml.list(dati)

cat(sprintf("UAI: M = %.1f  SD = %.1f  (N countries = %d)\n",
            uai_mean, uai_sd,
            length(unique(dati[[1]]$UAI[!is.na(dati[[1]]$UAI)]))))
cat("Italy UAI_z:", round((75 - uai_mean) / uai_sd, 3), "\n\n")

#' **Standardise remaining Hofstede dimensions at the country level** [EXTENSION]
#' Same procedure as UAI_z: z-score over the 27 unique country values.
hof_dims <- c("PDI", "IDV", "MAS", "LTO", "IVR")
for (dim in hof_dims) {
  dim_vals <- unique(dati[[1]][, c("cid", dim)])[[dim]]
  m <- mean(dim_vals, na.rm = TRUE)
  s <- sd(dim_vals,   na.rm = TRUE)
  zname <- paste0(dim, "_z")
  dati <- lapply(dati, function(x) {
    x[[zname]] <- (x[[dim]] - m) / s
    x
  })
  dati <- as.mitml.list(dati)
  cat(sprintf("%s: M = %.1f  SD = %.1f  Italy_%s = %.3f\n",
              dim, m, s, zname,
              (dati[[1]][dati[[1]]$cntry == "IT", dim][1] - m) / s))
}
cat("\n")
rm(hof_dims, dim, dim_vals, m, s, zname)




#' ===================================================================
#' # 2. Model A0: null model — ICC by wave [G&A + EXTENSION]
#' ===================================================================
#' Null models provide baseline ICC estimates before adding predictors.
#' Waves 1-3: rob (three-item composite).
#' Wave 4:    rob2item (two-item comparable composite).

cat("\n=== MODEL A0: NULL MODEL (ICC by wave) ===\n")

for (w in 1:3) {
  cat(sprintf("\nWave %d:\n", w))
  lmer.imp(rob ~ 1 + (1 | cid), data = dati, weights = "wgt2",
           subset = (dati[[1]]$wave == w))
}

cat("\nWave 4 (rob2item — two-item comparable composite):\n")
lmer.imp(rob2item ~ 1 + (1 | cid), data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 4))




#' ===================================================================
#' # 3. Model A1: individual-level predictors [G&A + wave 4]
#' ===================================================================
#' wave 1 = reference; wave 2, 3, 4 = contrasts vs. wave 1.
#' white 1 = reference (white-collar); white 2 = blue-collar;
#' white 3 = non-employed.
#' sex 0 = reference (male); sex 1 = female.

#' -------------------------------------------------------------------
#' ## 3a. Waves 1-3 (G&A replication — original model)
#' -------------------------------------------------------------------
cat("\n=== MODEL A1: INDIVIDUAL-LEVEL PREDICTORS (waves 1-3) ===\n")
cat("Replication of Gnambs & Appel (2019, Table 2)\n\n")

fit_A1_123 <- lmer.imp(
  rob ~ wave + sex + age + educ + white + (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE,
  subset  = (dati[[1]]$wave %in% c(1, 2, 3))
)


#' -------------------------------------------------------------------
#' ## 3b. Waves 1-4 [EXTENSION]
#' -------------------------------------------------------------------
#' rob2item is used as the dependent variable because rob3 wording
#' changed in wave 4, rendering the three-item composite non-comparable
#' across all four waves.
cat("\n=== MODEL A1: INDIVIDUAL-LEVEL PREDICTORS (waves 1-4) ===\n")
cat("[EXTENSION] Includes wave 4 (2024); dependent variable = rob2item\n\n")

fit_A1_1234 <- lmer.imp(
  rob2item ~ wave + sex + age + educ + white + (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE
)




#' ===================================================================
#' # 4. Model A2: individual + contextual predictors [G&A replication]
#' ===================================================================
#' Replication of G&A (2019, Table 3): adds AGEOLD, TECHEXP, INVEST,
#' UNEMP, LAT, LONG as Level-2 predictors.
#' G&A used waves 1-3; the extension adds wave 4.

#' -------------------------------------------------------------------
#' ## 4a. Waves 1-3 (G&A replication)
#' -------------------------------------------------------------------
cat("\n=== MODEL A2: L1 + L2 PREDICTORS (waves 1-3 — G&A replication) ===\n")

fit_A2_123 <- lmer.imp(
  rob ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
    (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE,
  subset  = (dati[[1]]$wave %in% c(1, 2, 3)),
  control = lmerControl(optimizer = "nloptwrap")
)

#' Fully standardised coefficients + conversion to Cohen's d (as in G&A)
cat("\n--- Fully standardised coefficients for L2 predictors ---\n")
fit_A2_123_std <- lmer.imp(
  rob ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
    (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = TRUE,
  subset  = (dati[[1]]$wave %in% c(1, 2, 3)),
  control = lmerControl(optimizer = "nloptwrap"),
  print   = FALSE
)
# Convert standardised betas to Cohen's d for L2 predictors (positions 9-14)
d_A2_123 <- round(2 * fit_A2_123_std$std[9:14] /
                    sqrt(1 - fit_A2_123_std$std[9:14]^2), 2)
cat("Cohen's d for L2 predictors (AGEOLD, TECHEXP, INVEST, UNEMP, LAT, LONG):\n")
print(d_A2_123)
rm(fit_A2_123_std)


#' -------------------------------------------------------------------
#' ## 4b. Waves 1-4 [EXTENSION]
#' -------------------------------------------------------------------
#' rob2item used as dependent variable for cross-wave comparability.
cat("\n=== MODEL A2: L1 + L2 PREDICTORS (waves 1-4) ===\n")
cat("[EXTENSION] Dependent variable = rob2item\n\n")

fit_A2_1234 <- lmer.imp(
  rob2item ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
    (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE,
  control = lmerControl(optimizer = "nloptwrap")
)




#' ===================================================================
#' # 5. Model B2 / B2_multi [EXTENSION — testing H1 and H2]
#' ===================================================================
#' Model B2      = A2 + UAI_z          (tests H1a and H2 in isolation)
#' Model B2_multi = A2 + all six Hofstede z-scores (tests full H1)
#'
#' H1a: UAI_z < 0 — uncertainty avoidance -> negative attitudes toward robots
#' H2:  the effect of LAT diminishes when UAI is added (cultural mediation).
#'
#' Caution for B2_multi: with N = 27 L2 units and 12 L2 predictors total
#' (6 G&A + 6 Hofstede), df at Level 2 is very low. Treat as exploratory;
#' focus interpretation on effect signs and relative magnitudes.

#' -------------------------------------------------------------------
#' ## 5a. Model B2 on waves 1-3
#' -------------------------------------------------------------------
cat("\n=== MODEL B2: A2 + UAI (waves 1-3) ===\n")
cat("[EXTENSION] Tests H1 (UAI main effect) and H2 (latitude mediation)\n\n")

fit_B2_123 <- lmer.imp(
  rob ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG + UAI_z +
    (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE,
  subset  = (dati[[1]]$wave %in% c(1, 2, 3)),
  control = lmerControl(optimizer = "nloptwrap")
)


#' -------------------------------------------------------------------
#' ## 5b. Model B2 on waves 1-4
#' -------------------------------------------------------------------
#' rob2item used as dependent variable for cross-wave comparability.
cat("\n=== MODEL B2: A2 + UAI (waves 1-4) ===\n")
cat("[EXTENSION] Dependent variable = rob2item\n\n")

fit_B2_1234 <- lmer.imp(
  rob2item ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG + UAI_z +
    (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE,
  control = lmerControl(optimizer = "nloptwrap")
)


#' -------------------------------------------------------------------
#' ## 5c. Model B2_multi: A2 + all Hofstede dimensions [EXTENSION — full H1]
#' -------------------------------------------------------------------
#' Tests H1 as a multidimensional cultural hypothesis.
#' Expected signs: UAI_z(-), IDV_z(+), LTO_z(+), IVR_z(+); PDI/MAS exploratory.

cat("\n=== MODEL B2_MULTI: A2 + ALL HOFSTEDE DIMENSIONS (waves 1-3) ===\n")
cat("[EXTENSION] Tests multidimensional H1; exploratory due to N=27 L2 units\n\n")

fit_B2multi_123 <- lmer.imp(
  rob ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
    PDI_z + IDV_z + MAS_z + UAI_z + LTO_z + IVR_z +
    (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE,
  subset  = (dati[[1]]$wave %in% c(1, 2, 3)),
  control = lmerControl(optimizer = "nloptwrap")
)

cat("\n=== MODEL B2_MULTI: A2 + ALL HOFSTEDE DIMENSIONS (waves 1-4) ===\n")
cat("[EXTENSION] Dependent variable = rob2item\n\n")

fit_B2multi_1234 <- lmer.imp(
  rob2item ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
    PDI_z + IDV_z + MAS_z + UAI_z + LTO_z + IVR_z +
    (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE,
  control = lmerControl(optimizer = "nloptwrap")
)


#' -------------------------------------------------------------------
#' ## 5d. Cross-level interactions [EXTENSION — testing moderation by UAI]
#' -------------------------------------------------------------------
#' H3a: Does UAI_z moderate the effect of education?
#'      (Education compensates cultural anxiety more strongly in high-UAI countries)
#' H3b: Does UAI_z moderate the effect of blue-collar status?
#'      (The negative effect of manual labour amplified by high UAI)
#'
#' rob2item used as dependent variable for cross-wave comparability.

cat("\n=== MODEL B3: CROSS-LEVEL INTERACTIONS (waves 1-4) ===\n")
cat("[EXTENSION] UAI x education and UAI x employment moderation\n\n")

fit_B3_1234 <- lmer.imp(
  rob2item ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG + UAI_z +
    UAI_z:educ +   # H3a: UAI moderates the education effect
    UAI_z:white +  # H3b: UAI moderates the employment effect
    (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE,
  control = lmerControl(optimizer = "nloptwrap")
)




#' ===================================================================
#' # 6. A2 vs. B2 comparison and Italian residuals
#' ===================================================================

cat("\n\n=== A2 vs. B2 COMPARISON: L2 VARIANCE EXPLAINED ===\n")

#' Extract variance components for comparison.
#' Note: AIC/BIC = Inf in lme4 with large weighted samples — a known
#' overflow issue in log-likelihood computation. Variance reduction
#' (pseudo-R2 at the country level) is used as an alternative.
#'
#' The residual analysis below uses the first imputed dataset (m=1).
#' This approximation is documented as a limitation: a fully pooled
#' estimate of BLUPs across all 20 imputed datasets is not feasible
#' with standard lme4 infrastructure and is therefore not computed here.

d1 <- dati[[1]]
d1$wave_num <- as.numeric(as.character(d1$wave))
d1_123 <- d1[d1$wave_num %in% 1:3, ]

# Normalised weights to avoid AIC/BIC overflow with large absolute weights
d1_123$wgt2_norm <- d1_123$wgt2 / mean(d1_123$wgt2, na.rm = TRUE)
d1$wgt2_norm     <- d1$wgt2     / mean(d1$wgt2,     na.rm = TRUE)

#' -------------------------------------------------------------------
#' ## 6a. Variance reduction — waves 1-3
#' -------------------------------------------------------------------
m_A2 <- lmer(rob ~ wave + sex + age + educ + white +
               AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
               (1 | cid),
             data    = d1_123,
             weights = wgt2_norm,
             REML    = FALSE,
             control = lmerControl(optimizer = "nloptwrap"))

m_B2 <- lmer(rob ~ wave + sex + age + educ + white +
               AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG + UAI_z +
               (1 | cid),
             data    = d1_123,
             weights = wgt2_norm,
             REML    = FALSE,
             control = lmerControl(optimizer = "nloptwrap"))

cat("\nL2 variance comparison A2 vs. B2 (waves 1-3, first imputed dataset):\n")
vc_A2 <- as.data.frame(VarCorr(m_A2))
vc_B2 <- as.data.frame(VarCorr(m_B2))
var_A2 <- vc_A2$vcov[vc_A2$grp == "cid"]
var_B2 <- vc_B2$vcov[vc_B2$grp == "cid"]
res_A2 <- vc_A2$vcov[vc_A2$grp == "Residual"]
res_B2 <- vc_B2$vcov[vc_B2$grp == "Residual"]
cat(sprintf("  Intercept variance A2: %.4f   ICC A2: %.4f\n",
            var_A2, var_A2 / (var_A2 + res_A2)))
cat(sprintf("  Intercept variance B2: %.4f   ICC B2: %.4f\n",
            var_B2, var_B2 / (var_B2 + res_B2)))
cat(sprintf("  Variance reduction at L2 by adding UAI: %.1f%%\n",
            (var_A2 - var_B2) / var_A2 * 100))


#' -------------------------------------------------------------------
#' ## 6b. Italian residuals — A2 vs. B2 (waves 1-3)
#' -------------------------------------------------------------------
#' If the Italian residual decreases from A2 to B2:
#'   -> UAI accounts for part of Italy's country-specific scepticism.
#' If the residual persists in B2:
#'   -> Italy's peculiarity is not captured by the Hofstede UAI dimension;
#'      country-specific mechanisms (collective memory, SME structure,
#'      industrial relations) are explored qualitatively in Chapter 6.

cat("\n=== ITALIAN RESIDUALS — A2 vs. B2 (waves 1-3) ===\n")

re_A2 <- ranef(m_A2)$cid
re_B2 <- ranef(m_B2)$cid

cid_cntry <- unique(d1_123[, c("cid", "cntry")])
re_A2$cid <- as.integer(rownames(re_A2))
re_B2$cid <- as.integer(rownames(re_B2))
re_A2 <- merge(re_A2, cid_cntry, by = "cid")
re_B2 <- merge(re_B2, cid_cntry, by = "cid")
names(re_A2)[2] <- "re_A2"
names(re_B2)[2] <- "re_B2"

re_compare <- merge(re_A2[, c("cntry", "re_A2")],
                    re_B2[, c("cntry", "re_B2")],
                    by = "cntry")
re_compare$change <- re_compare$re_B2 - re_compare$re_A2
re_compare <- re_compare[order(re_compare$re_A2), ]

cat("\nRandom effects by country (A2 vs. B2, waves 1-3):\n")
re_compare[, c("re_A2", "re_B2", "change")] <-
  round(re_compare[, c("re_A2", "re_B2", "change")], 3)
print(re_compare)

it_re <- re_compare[re_compare$cntry == "IT", ]
cat(sprintf("\n*** ITALY ***\n"))
cat(sprintf("  Residual Model A2: %+.3f\n", it_re$re_A2))
cat(sprintf("  Residual Model B2: %+.3f\n", it_re$re_B2))
cat(sprintf("  Change:            %+.3f\n", it_re$change))
cat(sprintf("  UAI accounts for %.1f%% of the Italian residual\n",
            abs(it_re$change / it_re$re_A2) * 100))


#' -------------------------------------------------------------------
#' ## 6c. Italian residuals — waves 1-4
#' -------------------------------------------------------------------
#' rob2item used as dependent variable for cross-wave comparability.
cat("\n--- Italian residuals — waves 1-4 (rob2item) ---\n")

m_A2_4 <- lmer(rob2item ~ wave + sex + age + educ + white +
                 AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
                 (1 | cid),
               data    = d1,
               weights = wgt2_norm,
               REML    = FALSE,
               control = lmerControl(optimizer = "nloptwrap"))

m_B2_4 <- lmer(rob2item ~ wave + sex + age + educ + white +
                 AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG + UAI_z +
                 (1 | cid),
               data    = d1,
               weights = wgt2_norm,
               REML    = FALSE,
               control = lmerControl(optimizer = "nloptwrap"))

re_A2_4 <- ranef(m_A2_4)$cid
re_B2_4 <- ranef(m_B2_4)$cid
re_A2_4$cid <- as.integer(rownames(re_A2_4))
re_B2_4$cid <- as.integer(rownames(re_B2_4))
cid_cntry4 <- unique(d1[, c("cid", "cntry")])
re_A2_4 <- merge(re_A2_4, cid_cntry4, by = "cid")
re_B2_4 <- merge(re_B2_4, cid_cntry4, by = "cid")
names(re_A2_4)[2] <- "re_A2"
names(re_B2_4)[2] <- "re_B2"
re_compare4 <- merge(re_A2_4[, c("cntry", "re_A2")],
                     re_B2_4[, c("cntry", "re_B2")], by = "cntry")
re_compare4$change <- re_compare4$re_B2 - re_compare4$re_A2
it_re4 <- re_compare4[re_compare4$cntry == "IT", ]
cat(sprintf("\n*** ITALY (waves 1-4, rob2item) ***\n"))
cat(sprintf("  Residual Model A2: %+.3f\n", it_re4$re_A2))
cat(sprintf("  Residual Model B2: %+.3f\n", it_re4$re_B2))
cat(sprintf("  Change:            %+.3f\n", it_re4$change))
if (it_re4$re_A2 != 0) {
  cat(sprintf("  UAI accounts for %.1f%% of the Italian residual\n",
              abs(it_re4$change / it_re4$re_A2) * 100))
}

#' -------------------------------------------------------------------
#' ## 6d. Variance reduction — A2 vs. B2_multi (waves 1-3)
#' -------------------------------------------------------------------
#' How much additional L2 variance do all six Hofstede dimensions explain
#' beyond the structural G&A predictors?

m_B2multi <- lmer(rob ~ wave + sex + age + educ + white +
                    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
                    PDI_z + IDV_z + MAS_z + UAI_z + LTO_z + IVR_z +
                    (1 | cid),
                  data    = d1_123,
                  weights = wgt2_norm,
                  REML    = FALSE,
                  control = lmerControl(optimizer = "nloptwrap"))

vc_Bm <- as.data.frame(VarCorr(m_B2multi))
var_Bm <- vc_Bm$vcov[vc_Bm$grp == "cid"]
res_Bm <- vc_Bm$vcov[vc_Bm$grp == "Residual"]
cat("\nL2 variance comparison A2 vs. B2_multi (waves 1-3):\n")
cat(sprintf("  Intercept variance B2_multi: %.4f   ICC: %.4f\n",
            var_Bm, var_Bm / (var_Bm + res_Bm)))
cat(sprintf("  Variance reduction vs. A2: %.1f%%  (vs. B2 UAI-only: %.1f%%)\n",
            (var_A2 - var_Bm) / var_A2 * 100,
            (var_B2 - var_Bm) / var_A2 * 100))

rm(m_A2, m_B2, m_B2multi, m_A2_4, m_B2_4)




#' ===================================================================
#' # 7. Feel items — Model A2 [G&A replication, waves 1-3]
#' ===================================================================
#' Model A2 replicated for each feel item (G&A Table 4).
#' Feel items are not available with a comparable scale in wave 4.

cat("\n\n=== MODEL A2 FEEL ITEMS (waves 1-3, G&A replication) ===\n")

cat("\n--- feel1: medical operation ---\n")
lmer.imp(feel1 ~ wave + sex + age + educ + white +
           AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
           (1 | cid),
         data    = dati,
         weights = "wgt2",
         stdy    = TRUE,
         stdx    = FALSE,
         subset  = (dati[[1]]$wave %in% c(1, 2, 3)),
         control = lmerControl(optimizer = "nloptwrap"))

cat("\n--- feel2: robots at work ---\n")
#' feel2 (qa8_3 wave 1 / qa7_2 wave 2 / qd13_2 wave 3) is available and
#' comparably labelled as "Assisting at work" across all three waves.
#' The G&A original script includes all three waves without restriction.
lmer.imp(feel2 ~ wave + sex + age + educ + white +
           AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
           (1 | cid),
         data    = dati,
         weights = "wgt2",
         stdy    = TRUE,
         stdx    = FALSE,
         subset  = (dati[[1]]$wave %in% c(1, 2, 3)),
         control = lmerControl(optimizer = "nloptwrap"))

cat("\n--- feel3: assisting elderly ---\n")
lmer.imp(feel3 ~ wave + sex + age + educ + white +
           AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
           (1 | cid),
         data    = dati,
         weights = "wgt2",
         stdy    = TRUE,
         stdx    = FALSE,
         subset  = (dati[[1]]$wave %in% c(2, 3)),
         control = lmerControl(optimizer = "nloptwrap"))

cat("\n--- feel4: driverless cars ---\n")
lmer.imp(feel4 ~ wave + sex + age + educ + white +
           AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
           (1 | cid),
         data    = dati,
         weights = "wgt2",
         stdy    = TRUE,
         stdx    = FALSE,
         subset  = (dati[[1]]$wave %in% c(2, 3)),
         control = lmerControl(optimizer = "nloptwrap"))




#' ===================================================================
#' # 8. Italy focus — within-country regression, wave 4 [EXTENSION]
#' ===================================================================
#' Single-country subsample (N ~ 1,037, wave 4).
#' lmer is not applicable for a single country (L2 = 1 unit).
#' Weighted linear regression with Rubin's pooling across m = 20
#' imputed datasets.
#'
#' rob2item used as dependent variable (two-item comparable composite,
#' range 0-6) for both descriptive comparability and internal consistency.
#'
#' Progressive block structure:
#'   Block 1 — sociodemographic (sex, age, educ)
#'   Block 2 — + employment type (white)
#'   Block 3 — EU vs. Italy comparison (wave 4)

cat("\n\n=== ITALY FOCUS — WITHIN-COUNTRY REGRESSION (wave 4) ===\n")
cat("[EXTENSION] Italian subsample, wave 4 (N ~ 1,037)\n\n")
cat("lmer not applicable for a single-country subsample (L2 = 1 unit).\n")
cat("Weighted lm with Rubin pooling across 20 imputed datasets.\n\n")

#' Helper: pool weighted lm results across a list of imputed datasets
pool_lm <- function(dati_list, formula_str, subset_expr, weight_var = "wgt2") {
  results <- lapply(dati_list, function(x) {
    sub <- x[eval(parse(text = subset_expr), envir = x), ]
    sub <- sub[complete.cases(sub[, all.vars(as.formula(formula_str))]), ]
    fit <- lm(as.formula(formula_str), data = sub,
              weights = sub[[weight_var]])
    list(coef = coef(fit), vcov = diag(vcov(fit)), n = nrow(sub))
  })
  qhat <- sapply(results, `[[`, "coef")
  uhat <- sapply(results, `[[`, "vcov")
  n    <- mean(sapply(results, `[[`, "n"))
  cat(sprintf("Mean N per imputed dataset: %.0f\n\n", n))
  testEstimates(qhat = qhat, uhat = uhat)
}


#' -------------------------------------------------------------------
#' ## 8a. Block 1: sociodemographic predictors
#' -------------------------------------------------------------------
cat("--- Block 1: Sociodemographic (sex, age, educ) ---\n")
pool_lm(dati,
        "rob2item ~ sex + age + educ",
        "wave == 4 & cntry == 'IT'")

#' -------------------------------------------------------------------
#' ## 8b. Block 2: + employment type
#' -------------------------------------------------------------------
cat("\n--- Block 2: + Employment type (white) ---\n")
pool_lm(dati,
        "rob2item ~ sex + age + educ + white",
        "wave == 4 & cntry == 'IT'")

#' -------------------------------------------------------------------
#' ## 8c. EU vs. Italy: individual-level effects in wave 4
#' -------------------------------------------------------------------
cat("\n--- EU vs. Italy: individual-level effects (wave 4) ---\n")
cat("(Coefficients standardised by Y)\n")

cat("\nEU27, wave 4 (rob2item):\n")
lmer.imp(rob2item ~ sex + age + educ + white + (1 | cid),
         data    = dati,
         weights = "wgt2",
         stdy    = TRUE,
         stdx    = FALSE,
         subset  = (dati[[1]]$wave == 4))

cat("\nItaly, wave 4 (rob2item, pooled lm):\n")
pool_lm(dati,
        "rob2item ~ sex + age + educ + white",
        "wave == 4 & cntry == 'IT'")


cat("\nScript 4 complete. Proceed to 5_Plots_extended.R\n")
