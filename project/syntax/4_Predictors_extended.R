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
#'   Section 5 — Italian residuals in Model A2 (waves 1-3 and 1-4)
#'   Section 6 — Feel items (G&A replication, waves 1-3)
#'   Section 7 — Italy focus, wave 4: within-country regression
#'   Section 8 — Wave-4 subscale analysis: rob2024_pos and rob2024_neg
#'
#' NOTE: Hofstede models (B2, B2_multi, B3, one-at-a-time, VIF) have been
#' archived in ./syntax/archive/Hofstede_explorations.R and are NOT part
#' of this pipeline. UAI showed p = 0.304 with 0.0% L2 variance reduction.
#'
#' Dependent variable note:
#'   Waves 1-3 analyses use rob (three-item composite, range 0-9).
#'   Waves 1-4 analyses use rob2item (two-item comparable composite,
#'   range 0-6), because rob3 wording changed in wave 4 ("boring/
#'   repetitive" vs. "hard/dangerous"), rendering rob non-comparable
#'   across all four waves.
#'   Section 8 uses rob2024_pos and rob2024_neg (wave-4 subscales) to
#'   probe the bidimensional structure of attitudes in 2024. [EXTENSION]


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

#' **Ensure rob2item and rob2024 subscales are available** [EXTENSION]
#' Passive-imputed variables recomputed here as a consistency safeguard.
dati <- lapply(dati, function(x) {
  x$rob2item <- x$rob1 + x$rob2
  if ("r24_c" %in% names(x)) {
    x$rob2024     <- ifelse(x$wave == 4,
                            x$rob2 + x$rob3 + x$r24_c + x$r24_d,
                            NA_real_)
    x$rob2024_pos <- ifelse(x$wave == 4, x$rob2 + x$rob3,    NA_real_)  # [EXTENSION]
    x$rob2024_neg <- ifelse(x$wave == 4, x$r24_c + x$r24_d, NA_real_)  # [EXTENSION]
  }
  x
})
dati <- as.mitml.list(dati)




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
#' # 5. Italian residuals — Model A2
#' ===================================================================
#' Random intercept for Italy in Model A2 (structural + individual predictors,
#' without Hofstede). Extracted from the first imputed dataset (m=1) using
#' normalised weights to avoid AIC/BIC overflow in log-likelihood computation.
#'
#' The A2 vs. B2 comparison that originally appeared here (showing that UAI
#' accounts for 0% of Italy's residual) has been archived in:
#' ./syntax/archive/Hofstede_explorations.R

d1 <- dati[[1]]
d1$wave_num <- as.numeric(as.character(d1$wave))
d1_123 <- d1[d1$wave_num %in% 1:3, ]

# Normalised weights to avoid AIC/BIC overflow with large absolute weights
d1_123$wgt2_norm <- d1_123$wgt2 / mean(d1_123$wgt2, na.rm = TRUE)
d1$wgt2_norm     <- d1$wgt2     / mean(d1$wgt2,     na.rm = TRUE)

#' -------------------------------------------------------------------
#' ## 5a. Waves 1-3 (rob)
#' -------------------------------------------------------------------
cat("\n=== ITALIAN RESIDUAL — MODEL A2 (waves 1-3) ===\n")

m_A2 <- lmer(rob ~ wave + sex + age + educ + white +
               AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
               (1 | cid),
             data    = d1_123,
             weights = wgt2_norm,
             REML    = FALSE,
             control = lmerControl(optimizer = "nloptwrap"))

re_A2 <- ranef(m_A2)$cid
cid_cntry <- unique(d1_123[, c("cid", "cntry")])
re_A2$cid <- as.integer(rownames(re_A2))
re_A2 <- merge(re_A2, cid_cntry, by = "cid")
names(re_A2)[2] <- "re_A2"
re_A2 <- re_A2[order(re_A2$re_A2), ]
re_A2$re_A2 <- round(re_A2$re_A2, 3)

cat("\nRandom effects by country (Model A2, waves 1-3):\n")
print(re_A2[, c("cntry", "re_A2")])

it_re <- re_A2[re_A2$cntry == "IT", ]
cat(sprintf("\n*** ITALY — Model A2, waves 1-3 ***\n"))
cat(sprintf("  Random intercept: %+.3f\n", it_re$re_A2))


#' -------------------------------------------------------------------
#' ## 5b. Waves 1-4 (rob2item)
#' -------------------------------------------------------------------
#' rob2item used as dependent variable for cross-wave comparability.
cat("\n--- Italian residual — Model A2 (waves 1-4, rob2item) ---\n")

m_A2_4 <- lmer(rob2item ~ wave + sex + age + educ + white +
                 AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
                 (1 | cid),
               data    = d1,
               weights = wgt2_norm,
               REML    = FALSE,
               control = lmerControl(optimizer = "nloptwrap"))

re_A2_4 <- ranef(m_A2_4)$cid
re_A2_4$cid <- as.integer(rownames(re_A2_4))
cid_cntry4 <- unique(d1[, c("cid", "cntry")])
re_A2_4 <- merge(re_A2_4, cid_cntry4, by = "cid")
names(re_A2_4)[2] <- "re_A2"
it_re4 <- re_A2_4[re_A2_4$cntry == "IT", ]

cat(sprintf("\n*** ITALY — Model A2, waves 1-4, rob2item ***\n"))
cat(sprintf("  Random intercept: %+.3f\n",
            round(it_re4$re_A2, 3)))

rm(m_A2, m_A2_4, d1, d1_123,
   re_A2, re_A2_4, cid_cntry, cid_cntry4, it_re, it_re4)




#' ===================================================================
#' # 6. Feel items — Model A2 [G&A replication, waves 1-3]
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
#' # 7. Italy focus — within-country regression, wave 4 [EXTENSION]
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

#' Helper: pool weighted lm results across a list of imputed datasets.
#' Uses print() to force output in both interactive and knitr contexts.
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
  print(testEstimates(qhat = qhat, uhat = uhat))
}


#' -------------------------------------------------------------------
#' ## 7a. Block 1: sociodemographic predictors
#' -------------------------------------------------------------------
cat("--- Block 1: Sociodemographic (sex, age, educ) ---\n")
pool_lm(dati,
        "rob2item ~ sex + age + educ",
        "wave == 4 & cntry == 'IT'")

#' -------------------------------------------------------------------
#' ## 7b. Block 2: + employment type
#' -------------------------------------------------------------------
cat("\n--- Block 2: + Employment type (white) ---\n")
pool_lm(dati,
        "rob2item ~ sex + age + educ + white",
        "wave == 4 & cntry == 'IT'")

#' -------------------------------------------------------------------
#' ## 7c. EU vs. Italy: individual-level effects in wave 4
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




#' ===================================================================
#' # 8. Wave-4 subscale analysis: rob2024_pos and rob2024_neg [EXTENSION]
#' ===================================================================
#' Psychometric validation (Script 2_1, Section 3b) rejected the 1-factor
#' structure for the four-item rob2024 composite (omega < .70; CFI < .95;
#' RMSEA > .06). A 2-factor CFA supports decomposition into two subscales:
#'   rob2024_pos = rob2 + rob3, range 0-6: "benefit/utility" dimension
#'                (social utility of automation + necessity for repetitive tasks)
#'   rob2024_neg = r24_c + r24_d, range 0-6: "absence-of-threat" dimension
#'                (items inverted; high = low perceived job-loss risk)
#'
#' Both are wave-4-only. The analyses mirror section 7 (Italy focus, wave 4),
#' producing parallel coefficients for the two dimensions. rob2024 is retained
#' in section 8e for descriptive comparison and is NOT used in inference.
#'
#' Substantive hypothesis [EXTENSION]:
#'   The occupational fracture (blue-collar effect) may be asymmetric across
#'   dimensions. Blue-collar workers are hypothesised to show a more negative
#'   coefficient on rob2024_neg (heightened job-loss threat perception) than
#'   on rob2024_pos (perceived utility of automation). If confirmed, this
#'   asymmetry implies that the widely observed occupation gradient in robot
#'   acceptance operates primarily through the threat dimension, not through
#'   utility perceptions. Treated as an exploratory hypothesis; not asserted.

cat("\n\n=== WAVE-4 SUBSCALE ANALYSIS: rob2024_pos and rob2024_neg ===\n")
cat("[EXTENSION] Two-subscale solution; 1-factor model rejected in Script 2_1\n")
cat("Hypothesis: blue-collar coefficient stronger (more negative) on rob2024_neg\n")
cat("            than on rob2024_pos — asymmetric occupational fracture.\n\n")

#' -------------------------------------------------------------------
#' ## 8a. EU27, wave 4 — rob2024_pos (benefit/utility) [EXTENSION]
#' -------------------------------------------------------------------
cat("--- EU27, wave 4 (rob2024_pos — benefit/utility, range 0-6) ---\n")
cat("(Coefficients standardised by Y)\n\n")
lmer.imp(rob2024_pos ~ sex + age + educ + white + (1 | cid), # [EXTENSION]
         data    = dati,                                      # [EXTENSION]
         weights = "wgt2",                                    # [EXTENSION]
         stdy    = TRUE,                                      # [EXTENSION]
         stdx    = FALSE,                                     # [EXTENSION]
         subset  = (dati[[1]]$wave == 4))                     # [EXTENSION]

#' -------------------------------------------------------------------
#' ## 8b. EU27, wave 4 — rob2024_neg (absence of threat) [EXTENSION]
#' -------------------------------------------------------------------
cat("\n--- EU27, wave 4 (rob2024_neg — absence of threat, range 0-6) ---\n")
cat("(Coefficients standardised by Y)\n\n")
lmer.imp(rob2024_neg ~ sex + age + educ + white + (1 | cid), # [EXTENSION]
         data    = dati,                                      # [EXTENSION]
         weights = "wgt2",                                    # [EXTENSION]
         stdy    = TRUE,                                      # [EXTENSION]
         stdx    = FALSE,                                     # [EXTENSION]
         subset  = (dati[[1]]$wave == 4))                     # [EXTENSION]

cat("\n[Compare 8a vs. 8b: is the blue-collar coefficient",    # [EXTENSION]
    "larger on rob2024_neg?]\n")                              # [EXTENSION]

#' -------------------------------------------------------------------
#' ## 8c. Italy, wave 4 — rob2024_pos [EXTENSION]
#' -------------------------------------------------------------------
cat("\n--- Italy, wave 4 (rob2024_pos, pooled lm) ---\n")
pool_lm(dati,                                                # [EXTENSION]
        "rob2024_pos ~ sex + age + educ + white",            # [EXTENSION]
        "wave == 4 & cntry == 'IT'")                        # [EXTENSION]

#' -------------------------------------------------------------------
#' ## 8d. Italy, wave 4 — rob2024_neg [EXTENSION]
#' -------------------------------------------------------------------
cat("\n--- Italy, wave 4 (rob2024_neg, pooled lm) ---\n")
pool_lm(dati,                                                # [EXTENSION]
        "rob2024_neg ~ sex + age + educ + white",            # [EXTENSION]
        "wave == 4 & cntry == 'IT'")                        # [EXTENSION]

cat("\n[Compare 8c vs. 8d: Italy-specific asymmetry",          # [EXTENSION]
    "between threat and utility dimensions?]\n")              # [EXTENSION]

#' -------------------------------------------------------------------
#' ## 8e. rob2024 retained as descriptive reference [EXTENSION]
#' -------------------------------------------------------------------
#' rob2024 (4-item composite, range 0-12) is not used for inferential
#' conclusions given the rejected 1-factor measurement structure.
#' The mean comparison below verifies consistency with the subscale findings
#' and provides a descriptive anchor for the methodological appendix.
cat("\n--- Descriptive reference: mean comparison wave 4 ---\n") # [EXTENSION]
cat("  [rob2item 0-6; rob2024 0-12; rob2024_pos/neg 0-6;",      # [EXTENSION]
    "means as % of range]\n\n")                                  # [EXTENSION]
for (sub_lbl in c("EU27", "Italy")) {                            # [EXTENSION]
  mask <- if (sub_lbl == "Italy")                                # [EXTENSION]
    dati[[1]]$wave == 4 & dati[[1]]$cntry == "IT"               # [EXTENSION]
  else                                                           # [EXTENSION]
    dati[[1]]$wave == 4                                          # [EXTENSION]
  .m2i  <- mean(sapply(dati, function(x)                        # [EXTENSION]
    mean(x$rob2item[mask],    na.rm = TRUE)))                    # [EXTENSION]
  .m24  <- mean(sapply(dati, function(x)                        # [EXTENSION]
    mean(x$rob2024[mask],     na.rm = TRUE)))                    # [EXTENSION]
  .mpos <- mean(sapply(dati, function(x)                        # [EXTENSION]
    mean(x$rob2024_pos[mask], na.rm = TRUE)))                    # [EXTENSION]
  .mneg <- mean(sapply(dati, function(x)                        # [EXTENSION]
    mean(x$rob2024_neg[mask], na.rm = TRUE)))                    # [EXTENSION]
  cat(sprintf(                                                   # [EXTENSION]
    "  %s: rob2item=%.2f(%.0f%%)  rob2024=%.2f(%.0f%%)",        # [EXTENSION]
    sub_lbl, .m2i, .m2i/6*100, .m24, .m24/12*100))             # [EXTENSION]
  cat(sprintf(                                                   # [EXTENSION]
    "  pos=%.2f(%.0f%%)  neg=%.2f(%.0f%%)\n",                   # [EXTENSION]
    .mpos, .mpos/6*100, .mneg, .mneg/6*100))                    # [EXTENSION]
}                                                                # [EXTENSION]
rm(.m2i, .m24, .mpos, .mneg, mask, sub_lbl)                     # [EXTENSION]


cat("\nScript 4 complete. Proceed to 5_Plots_extended.R\n")
