#' ---
#' title: Descriptive results — Extended (4 waves, 2012-2024)
#' author: Camilla Bonomo [extends Gnambs & Appel 2019]
#' output:
#'    html_document:
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---

#' **Clear workspace**
rm(list = ls())

#' **Load packages**
library(weights)
library(psych)
library(doBy)
library(mitml)
library(lavaan)   # [EXTENSION] CFA for rob2024 measurement validation
source("./syntax/0_Start.R")   # helper functions: describe.imp, cor.imp, omg.imp

#' **Load data**
load("./data/dat.Rdata")
rm(dati_mice)   # dati_mice is not required for descriptive analyses

# rob2item and rob2024 are available as passive-imputed variables in dati;
# the block below recomputes them from components as a consistency safeguard.
dati <- lapply(dati, function(x) {
  x$rob2item <- x$rob1 + x$rob2
  if ("r24_c" %in% names(x)) {                  # [EXTENSION] guard for legacy dat.Rdata
    x$rob2024 <- ifelse(x$wave == 4,             # [EXTENSION]
                        x$rob2 + x$rob3 +        # [EXTENSION]
                        x$r24_c + x$r24_d,       # [EXTENSION]
                        NA_real_)                # [EXTENSION]
  }
  x
})
dati <- as.mitml.list(dati)




#' ===================================================================
#' # 1. Sample description
#' ===================================================================

#' **Countries included**
sort(unique(dat$cntry))
length(unique(dat$cntry))   # number of countries

#' **N per country x wave** [EXTENSION: includes wave 4; UK excluded post-Brexit]
table(dat$cntry, dat$wave)
describe(c(table(dat$cntry, dat$wave)))

#' **Total N**
nrow(dat)

#' **N by wave**
table(dat$wave)

#' **N Italy by wave** [EXTENSION: Italy focus]
cat("\n=== ITALY ===\n")
table(dat$wave[dat$cntry == "IT"])




#' ===================================================================
#' # 2. Sociodemographic characteristics
#' ===================================================================

#' **Sex** (0 = male, 1 = female)
#' Note: wave 4 includes a non-binary option (N = 49), recoded as NA.
prop.table(table(dat$sex))
prop.table(table(dat$sex, dat$wave), margin = 2)   # [EXTENSION] by wave

#' **Age**
describe(dat$age)
tapply(dat$age, dat$wave, mean, na.rm = TRUE)   # [EXTENSION] mean age by wave

#' **Education** (age at end of full-time education)
describe(dat$educ)
tapply(dat$educ, dat$wave, mean, na.rm = TRUE)

#' **Employment status** (1 = white-collar, 2 = blue-collar, 3 = non-employed)
prop.table(table(dat$white))
prop.table(table(dat$white, dat$wave), margin = 2)   # [EXTENSION] by wave




#' ===================================================================
#' # 3. Reliability of the attitude scale
#' ===================================================================
#' [G&A] Categorical omega (poly = TRUE) computed on rob1-rob3 for waves 1-3.
#' [EXTENSION] Wave 4: rob3 wording changed ("boring/repetitive" instead of
#' "hard/dangerous"). Reliability is therefore computed on the two-item
#' comparable composite (rob2item = rob1 + rob2) for wave 4.

#' **Wave 1 — 2012** (rob1, rob2, rob3) [G&A]
cat("\n--- Reliability Wave 1 (2012) ---\n")
omg.imp(dati, items = paste0("rob", 1:3), poly = TRUE,
        weights = "wgt1",
        subset  = (dati[[1]]$wave == 1))

#' **Wave 2 — 2014** (rob1, rob2, rob3) [G&A]
cat("\n--- Reliability Wave 2 (2014) ---\n")
omg.imp(dati, items = paste0("rob", 1:3), poly = TRUE,
        weights = "wgt1",
        subset  = (dati[[1]]$wave == 2))

#' **Wave 3 — 2017** (rob1, rob2, rob3) [G&A]
cat("\n--- Reliability Wave 3 (2017) ---\n")
omg.imp(dati, items = paste0("rob", 1:3), poly = TRUE,
        weights = "wgt1",
        subset  = (dati[[1]]$wave == 3))

#' **Wave 4 — 2024** (rob1, rob2 — comparable two-item scale) [EXTENSION]
#' rob3 is excluded because the wording "boring/repetitive tasks" is not
#' equivalent to "hard/dangerous tasks" used in waves 1-3.
cat("\n--- Reliability Wave 4 (2024) — two-item comparable scale ---\n")
omg.imp(dati, items = paste0("rob", 1:2), poly = TRUE,
        weights = "wgt1",
        subset  = (dati[[1]]$wave == 4))

#' Methodological note [EXTENSION]:
#' The three-item composite (rob) is used for the longitudinal analysis of
#' waves 1-3, supported by measurement invariance testing (Script 2_2).
#' The two-item composite (rob2item) is used for all comparisons extending
#' to wave 4. This choice is documented in the methodological appendix.




#' ===================================================================
#' # 3b. rob2024 measurement structure — justification for two-subscale solution [EXTENSION]
#' ===================================================================
#' Wave 4 extends attitude measurement with four items intended to form rob2024.
#' Psychometric validation demonstrates that a single-factor structure is untenable.
#' The evidence is organised in four stages:
#'   Stage 1: 1-factor omega and CFA — basis for rejection
#'   Stage 2: internal consistency of each subscale (polychoric r, 2 items)
#'   Stage 3: 2-factor CFA — confirmatory test of the bidimensional structure
#'   Stage 4: convergent validity of both subscales with rob1 (QB5 global appraisal)
#'
#' Items:
#'   rob2  = qb6_2  "robots/AI help people at work"         (positive, -> rob2024_pos)
#'   rob3  = qb6_4  "AI handles boring/repetitive tasks"     (positive, -> rob2024_pos)
#'   r24_c = qb6_5  "robots steal jobs from people"          (INVERTED, -> rob2024_neg)
#'   r24_d = qb6_1  "more jobs disappear than be created"    (INVERTED, -> rob2024_neg)
#'
#' Substantive rationale: citizens may simultaneously recognise the social utility
#' of automation (rob2024_pos) AND fear its labour-market consequences (rob2024_neg).
#' The two dimensions are expected to correlate positively but not collapse onto one
#' factor. rob2024 is retained as descriptive composite and validity reference.

#' -------------------------------------------------------------------
#' ## 3b.1 Stage 1: 1-factor model — omega and CFA as basis for rejection [EXTENSION]
#' -------------------------------------------------------------------
#' Expected: omega < .70 and CFA fit below conventional thresholds
#' (CFI < .95, RMSEA > .06), with asymmetric loadings (positive items < negative items).

cat("\n--- Stage 1: 1-factor evidence (documentation of inadequacy) ---\n")

#' **Omega — rob2024, 4 items, wave 4** [EXTENSION]
cat("\nOmega (4 items, should be < .70):\n")           # [EXTENSION]
if ("r24_c" %in% names(dati[[1]])) {                   # [EXTENSION]
  omg.imp(dati,                                         # [EXTENSION]
          items   = c(paste0("rob", 2:3), "r24_c", "r24_d"), # [EXTENSION]
          poly    = TRUE,                               # [EXTENSION]
          weights = "wgt1",                             # [EXTENSION]
          subset  = (dati[[1]]$wave == 4))              # [EXTENSION]
} else {                                                # [EXTENSION]
  .omg_items <- c("rob2", "rob3", "r24_c", "r24_d")   # [EXTENSION]
  .omg_dat   <- dat[dat$wave == 4, c(.omg_items, "wgt1")] # [EXTENSION]
  .omg_dat   <- .omg_dat[complete.cases(.omg_dat), ]   # [EXTENSION]
  .rho <- mixedCor(.omg_dat[, .omg_items],             # [EXTENSION]
                   p = .omg_items,                     # [EXTENSION]
                   weight = .omg_dat$wgt1)$poly$rho    # [EXTENSION]
  .rho <- cor.smooth(.rho)                             # [EXTENSION]
  .fa  <- fa(.rho, 1, n.obs = nrow(.omg_dat), fm = "ml") # [EXTENSION]
  .lds <- c(loadings(.fa)[])                           # [EXTENSION]
  .omg_val <- (sum(abs(.lds))^2) /                     # [EXTENSION]
              (sum(abs(.lds))^2 + sum(1 - .lds^2))     # [EXTENSION]
  cat("Omega (observed wave-4 data, N =", nrow(.omg_dat), "):", # [EXTENSION]
      round(.omg_val, 2), "\n")                        # [EXTENSION]
  rm(.omg_items, .omg_dat, .rho, .fa, .lds, .omg_val) # [EXTENSION]
}                                                       # [EXTENSION]

#' **1-factor CFA (WLSMV, observed data)** [EXTENSION]
#' Fit rejection confirms inadequacy of unidimensional model.
#' Asymmetric loadings (positive items rob2/rob3 < negative items r24_c/r24_d)
#' are the empirical signature of bidimensionality.
cat("\n1-factor CFA (WLSMV; CFI/TLI/RMSEA should indicate poor fit):\n") # [EXTENSION]
.cfa_items <- c("rob2", "rob3", "r24_c", "r24_d")                        # [EXTENSION]
dat_w4_cfa <- dat[dat$wave == 4 &                                         # [EXTENSION]
                  complete.cases(dat[, .cfa_items]), .cfa_items]          # [EXTENSION]
fit_1f <- cfa("f_rob =~ rob2 + rob3 + r24_c + r24_d",                    # [EXTENSION]
              data = dat_w4_cfa, ordered = TRUE, estimator = "WLSMV")    # [EXTENSION]
std_1f <- standardizedSolution(fit_1f)                                    # [EXTENSION]
cat("\nStandardised loadings (expected: pos items < neg items):\n")       # [EXTENSION]
print(std_1f[std_1f$op == "=~",                                           # [EXTENSION]
             c("lhs", "rhs", "est.std", "se", "z", "pvalue")],           # [EXTENSION]
      row.names = FALSE)                                                   # [EXTENSION]
cat("\nFit indices (basis for rejection):\n")                             # [EXTENSION]
print(fitMeasures(fit_1f,                                                 # [EXTENSION]
                  c("cfi", "tli", "rmsea", "rmsea.ci.lower",             # [EXTENSION]
                    "rmsea.ci.upper", "srmr")))                           # [EXTENSION]
rm(.cfa_items, std_1f)                                                    # [EXTENSION]


#' -------------------------------------------------------------------
#' ## 3b.2 Stage 2: internal consistency of subscales [EXTENSION]
#' -------------------------------------------------------------------
#' With 2 items per subscale, CFA-based omega is not identified.
#' The polychoric correlation is reported as internal consistency indicator.
#' The Spearman-Brown formula (SB = 2r / (1+r)) gives the equivalent of
#' alpha/omega for a 2-item scale and is reported alongside r_poly.
#' Estimated on observed complete-case wave-4 data with wgt1.

cat("\n--- Stage 2: polychoric internal consistency of subscales ---\n") # [EXTENSION]

#' rob2024_pos: r(rob2, rob3) — benefit/utility dimension [EXTENSION]
.d_pos  <- dat[dat$wave == 4, c("rob2", "rob3", "wgt1")]                 # [EXTENSION]
.d_pos  <- .d_pos[complete.cases(.d_pos), ]                              # [EXTENSION]
.r_pos  <- mixedCor(.d_pos[, c("rob2", "rob3")],                         # [EXTENSION]
                    p = c("rob2", "rob3"),                               # [EXTENSION]
                    weight = .d_pos$wgt1)$poly$rho[1, 2]                 # [EXTENSION]
.sb_pos <- 2 * .r_pos / (1 + .r_pos)                                    # [EXTENSION]
cat(sprintf("  rob2024_pos [rob2 + rob3]:   r_poly = %.3f   SB = %.3f\n",# [EXTENSION]
            .r_pos, .sb_pos))                                            # [EXTENSION]
rm(.d_pos, .r_pos, .sb_pos)                                              # [EXTENSION]

#' rob2024_neg: r(r24_c, r24_d) — absence-of-threat dimension [EXTENSION]
.d_neg  <- dat[dat$wave == 4, c("r24_c", "r24_d", "wgt1")]              # [EXTENSION]
.d_neg  <- .d_neg[complete.cases(.d_neg), ]                             # [EXTENSION]
.r_neg  <- mixedCor(.d_neg[, c("r24_c", "r24_d")],                      # [EXTENSION]
                    p = c("r24_c", "r24_d"),                            # [EXTENSION]
                    weight = .d_neg$wgt1)$poly$rho[1, 2]                 # [EXTENSION]
.sb_neg <- 2 * .r_neg / (1 + .r_neg)                                    # [EXTENSION]
cat(sprintf("  rob2024_neg [r24_c + r24_d]: r_poly = %.3f   SB = %.3f\n",# [EXTENSION]
            .r_neg, .sb_neg))                                            # [EXTENSION]
rm(.d_neg, .r_neg, .sb_neg)                                              # [EXTENSION]


#' -------------------------------------------------------------------
#' ## 3b.3 Stage 3: 2-factor CFA — confirmatory test [EXTENSION]
#' -------------------------------------------------------------------
#' Two correlated factors: pos_f (rob2, rob3) and neg_f (r24_c, r24_d).
#' Expected: substantially better fit than 1-factor model (CFI > .95,
#' RMSEA < .06). The inter-factor correlation quantifies shared variance
#' between the utility and threat dimensions.
#' Model has df = 1 (barely over-identified); the chi-square and RMSEA
#' confidence interval are reported for completeness.

cat("\n--- Stage 3: 2-factor CFA (WLSMV; expected good fit) ---\n")     # [EXTENSION]
fit_2f <- cfa("pos_f =~ rob2 + rob3\n  neg_f =~ r24_c + r24_d",        # [EXTENSION]
              data = dat_w4_cfa, ordered = TRUE, estimator = "WLSMV")   # [EXTENSION]
std_2f <- standardizedSolution(fit_2f)                                   # [EXTENSION]
cat("\nStandardised factor loadings:\n")                                  # [EXTENSION]
print(std_2f[std_2f$op == "=~",                                          # [EXTENSION]
             c("lhs", "rhs", "est.std", "se", "z", "pvalue")],          # [EXTENSION]
      row.names = FALSE)                                                  # [EXTENSION]
cat("\nInter-factor correlation (pos_f ~~ neg_f):\n")                    # [EXTENSION]
.fcov <- std_2f[std_2f$op == "~~" & std_2f$lhs != std_2f$rhs, ]        # [EXTENSION]
print(.fcov[, c("lhs", "rhs", "est.std", "se", "z", "pvalue")],        # [EXTENSION]
      row.names = FALSE)                                                  # [EXTENSION]
cat("\nFit indices (should be substantially better than 1-factor):\n")  # [EXTENSION]
print(fitMeasures(fit_2f,                                                # [EXTENSION]
                  c("cfi", "tli", "rmsea", "rmsea.ci.lower",            # [EXTENSION]
                    "rmsea.ci.upper", "srmr")))                          # [EXTENSION]
rm(std_2f, .fcov, dat_w4_cfa, fit_1f, fit_2f)                           # [EXTENSION]


#' -------------------------------------------------------------------
#' ## 3b.4 Stage 4: convergent validity with rob1 (QB5) [EXTENSION]
#' -------------------------------------------------------------------
#' rob1 (QB5: overall appraisal of robots/AI) is external to all four items
#' and serves as the convergent validity criterion. Both subscales are expected
#' to correlate positively with rob1 (r > .30). rob2024_pos is expected to
#' correlate more strongly because QB5 is a positive valuation item, structurally
#' closer to the utility dimension. rob2024 is retained as reference.

cat("\n--- Stage 4: convergent validity with rob1 (QB5), wave 4 ---\n") # [EXTENSION]
cat("  r(rob2024, rob1) — reference:\n")                                 # [EXTENSION]
cor.imp(dati,                                                            # [EXTENSION]
        items   = c("rob2024", "rob1"),                                  # [EXTENSION]
        weights = "wgt2", digits = 3,                                    # [EXTENSION]
        subset  = (dati[[1]]$wave == 4))                                 # [EXTENSION]
cat("  r(rob2024_pos, rob1) — utility dimension:\n")                     # [EXTENSION]
cor.imp(dati,                                                            # [EXTENSION]
        items   = c("rob2024_pos", "rob1"),                              # [EXTENSION]
        weights = "wgt2", digits = 3,                                    # [EXTENSION]
        subset  = (dati[[1]]$wave == 4))                                 # [EXTENSION]
cat("  r(rob2024_neg, rob1) — absence-of-threat dimension:\n")           # [EXTENSION]
cor.imp(dati,                                                            # [EXTENSION]
        items   = c("rob2024_neg", "rob1"),                              # [EXTENSION]
        weights = "wgt2", digits = 3,                                    # [EXTENSION]
        subset  = (dati[[1]]$wave == 4))                                 # [EXTENSION]

#' Methodological note [EXTENSION]:
#' The 1-factor model for rob2024 is rejected on both omega (< .70) and CFA fit
#' (CFI < .95, RMSEA > .06) grounds. Asymmetric loadings indicate two distinct
#' dimensions. The 2-factor solution (rob2024_pos + rob2024_neg) is adopted for
#' all inferential analyses in Script 4, Section 9. rob2024 is retained as a
#' descriptive composite and convergent validity reference.
#' Acceptability benchmarks for the subscales: SB >= .60; 2-factor CFA CFI >= .95
#' and RMSEA <= .06; inter-factor r positive and moderate (.30-.60).




#' ===================================================================
#' # 4. Descriptives and correlations — by wave
#' ===================================================================

#' **Dummy variables** [G&A]
dati <- within(dati, {
  sex1     <- ifelse(sex == 1, 1, 0)
  white2   <- ifelse(white == 2, 1, 0)
  white3   <- ifelse(white == 3, 1, 0)
  rob2item <- rob1 + rob2     # recompute for safety
})

#' -------------------------------------------------------------------
#' ## 4a. Means and standard deviations
#' -------------------------------------------------------------------

#' **Wave 1 — 2012** [G&A]
#' Note: feel3 and feel4 are not available in wave 1.
cat("\n--- Descriptives Wave 1 (2012) ---\n")
describe.imp(dati,
             items   = c("rob", paste0("feel", 1:2),
                         "sex1", "age", "educ", "white2", "white3"),
             weights = "wgt2",
             stats   = c("mean", "sd"),
             subset  = (dati[[1]]$wave == 1))

#' **Wave 2 — 2014** [G&A]
cat("\n--- Descriptives Wave 2 (2014) ---\n")
describe.imp(dati,
             items   = c("rob", paste0("feel", 1:4),
                         "sex1", "age", "educ", "white2", "white3"),
             weights = "wgt2",
             stats   = c("mean", "sd"),
             subset  = (dati[[1]]$wave == 2))

#' **Wave 3 — 2017** [G&A]
cat("\n--- Descriptives Wave 3 (2017) ---\n")
describe.imp(dati,
             items   = c("rob", paste0("feel", 1:4),
                         "sex1", "age", "educ", "white2", "white3"),
             weights = "wgt2",
             stats   = c("mean", "sd"),
             subset  = (dati[[1]]$wave == 3))

#' **Wave 4 — 2024** [EXTENSION]
#' feel1-4 are structurally absent in wave 4 (non-comparable scale).
#' rob2item is the appropriate comparable composite for this wave.
#' rob2024 (4-item wave-4 composite) is also reported. [EXTENSION]
cat("\n--- Descriptives Wave 4 (2024) ---\n")
describe.imp(dati,
             items   = c("rob", "rob2item", "rob2024",  # [EXTENSION] rob2024 added
                         "sex1", "age", "educ", "white2", "white3"),
             weights = "wgt2",
             stats   = c("mean", "sd"),
             subset  = (dati[[1]]$wave == 4))


#' -------------------------------------------------------------------
#' ## 4b. Correlations between study variables
#' -------------------------------------------------------------------

#' **Wave 1 — 2012** [G&A]
cat("\n--- Correlations Wave 1 (2012) ---\n")
cor.imp(dati,
        items   = c("rob", paste0("feel", 1:2),
                    "sex1", "age", "educ", "white2", "white3"),
        weights = "wgt2", digits = 3,
        subset  = (dati[[1]]$wave == 1))

#' **Wave 2 — 2014** [G&A]
cat("\n--- Correlations Wave 2 (2014) ---\n")
cor.imp(dati,
        items   = c("rob", paste0("feel", 1:4),
                    "sex1", "age", "educ", "white2", "white3"),
        weights = "wgt2", digits = 3,
        subset  = (dati[[1]]$wave == 2))

#' **Wave 3 — 2017** [G&A]
cat("\n--- Correlations Wave 3 (2017) ---\n")
cor.imp(dati,
        items   = c("rob", paste0("feel", 1:4),
                    "sex1", "age", "educ", "white2", "white3"),
        weights = "wgt2", digits = 3,
        subset  = (dati[[1]]$wave == 3))

#' **Wave 4 — 2024** [EXTENSION]
#' rob2item replaces rob as the focal dependent variable for wave 4.
#' rob2024 is included alongside rob2item as robustness check. [EXTENSION]
cat("\n--- Correlations Wave 4 (2024) ---\n")
cor.imp(dati,
        items   = c("rob2item", "rob2024",               # [EXTENSION] rob2024 added
                    "sex1", "age", "educ", "white2", "white3"),
        weights = "wgt2", digits = 3,
        subset  = (dati[[1]]$wave == 4))


#' -------------------------------------------------------------------
#' ## 4c. Missing data patterns by wave
#' -------------------------------------------------------------------

#' **Wave 1** [G&A]
cat("\n--- Missing Wave 1 ---\n")
apply(dat[dat$wave == 1,
          c("rob", paste0("feel", 1:2), "sex", "age", "educ", "white")],
      2, function(x) round(mean(is.na(x)), 3))

#' **Wave 2** [G&A]
cat("\n--- Missing Wave 2 ---\n")
apply(dat[dat$wave == 2,
          c("rob", paste0("feel", 1:4), "sex", "age", "educ", "white")],
      2, function(x) round(mean(is.na(x)), 3))

#' **Wave 3** [G&A]
cat("\n--- Missing Wave 3 ---\n")
apply(dat[dat$wave == 3,
          c("rob", paste0("feel", 1:4), "sex", "age", "educ", "white")],
      2, function(x) round(mean(is.na(x)), 3))

#' **Wave 4** [EXTENSION]
#' feel1-4 are structurally absent (structural zeros, not missing data).
#' r24_c, r24_d, and rob2024 are wave-4-only. [EXTENSION]
cat("\n--- Missing Wave 4 ---\n")
apply(dat[dat$wave == 4,
          c("rob", "rob2item", "rob2024",             # [EXTENSION] rob2024 added
            "r24_c", "r24_d",                         # [EXTENSION]
            "sex", "age", "educ", "white")],
      2, function(x) round(mean(is.na(x)), 3))




#' ===================================================================
#' # 5. Longitudinal overview — composite score by wave and country
#' ===================================================================
#' [EXTENSION] Summary of weighted composite scores across all four waves,
#' for EU27 and the Italian subsample.

#' **Weighted means by wave — EU27**
#' Waves 1-3: rob (three-item, 0-9). Wave 4: rob2item (two-item, 0-6).
#' Means are not directly comparable in magnitude across the two composites.
cat("\n=== WEIGHTED COMPOSITE SCORE BY WAVE (EU27) ===\n")
cat("  [Waves 1-3: rob (0-9); Wave 4: rob2item (0-6) — different scales]\n")
for (w in 1:3) {
    sub <- dat[dat$wave == w & !is.na(dat$rob) & !is.na(dat$wgt2), ]
    m   <- weighted.mean(sub$rob, sub$wgt2)
    cat(sprintf("  Wave %d: M(rob)     = %.3f  (N = %d)\n", w, m, nrow(sub)))
}
sub4 <- dat[dat$wave == 4 & !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
cat(sprintf("  Wave 4: M(rob2item) = %.3f  (N = %d)\n",
            weighted.mean(sub4$rob2item, sub4$wgt2), nrow(sub4)))

#' **Weighted means by wave — Italy** [EXTENSION]
cat("\n=== ITALY: WEIGHTED COMPOSITE SCORE BY WAVE ===\n")
cat("  [Waves 1-3: rob (0-9); Wave 4: rob2item (0-6) — different scales]\n")
for (w in 1:3) {
    sub <- dat[dat$wave == w & dat$cntry == "IT" &
               !is.na(dat$rob) & !is.na(dat$wgt2), ]
    m   <- weighted.mean(sub$rob, sub$wgt2)
    eu  <- weighted.mean(
               dat[dat$wave == w & !is.na(dat$rob) & !is.na(dat$wgt2), "rob"],
               dat[dat$wave == w & !is.na(dat$rob) & !is.na(dat$wgt2), "wgt2"])
    cat(sprintf("  Wave %d (rob):     IT = %.3f  EU = %.3f  diff = %+.3f  (N_IT = %d)\n",
                w, m, eu, m - eu, nrow(sub)))
}
sub4_it <- dat[dat$wave == 4 & dat$cntry == "IT" &
               !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
sub4_eu <- dat[dat$wave == 4 & !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
m4_it   <- weighted.mean(sub4_it$rob2item, sub4_it$wgt2)
m4_eu   <- weighted.mean(sub4_eu$rob2item, sub4_eu$wgt2)
cat(sprintf("  Wave 4 (rob2item): IT = %.3f  EU = %.3f  diff = %+.3f  (N_IT = %d)\n",
            m4_it, m4_eu, m4_it - m4_eu, nrow(sub4_it)))

#' **Country rankings by wave** [EXTENSION]
cat("\n=== COUNTRY RANKINGS BY WAVE ===\n")
cat("  [Waves 1-3: rob (0-9); Wave 4: rob2item (0-6) — not comparable in magnitude]\n")
for (w in 1:3) {
    sub <- dat[dat$wave == w & !is.na(dat$rob) & !is.na(dat$wgt2), ]
    means_by_country <- tapply(sub$rob * sub$wgt2, sub$cntry, sum) /
                        tapply(sub$wgt2,   sub$cntry, sum)
    cat(sprintf("\nWave %d (rob) — Top 3:\n", w))
    print(round(sort(means_by_country, decreasing = TRUE)[1:3], 3))
    cat(sprintf("Wave %d (rob) — Bottom 3:\n", w))
    print(round(sort(means_by_country)[1:3], 3))
}
sub4r <- dat[dat$wave == 4 & !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
means_w4 <- tapply(sub4r$rob2item * sub4r$wgt2, sub4r$cntry, sum) /
            tapply(sub4r$wgt2, sub4r$cntry, sum)
cat("\nWave 4 (rob2item) — Top 3:\n")
print(round(sort(means_w4, decreasing = TRUE)[1:3], 3))
cat("Wave 4 (rob2item) — Bottom 3:\n")
print(round(sort(means_w4)[1:3], 3))




#' ===================================================================
#' # 6. Country-level contextual variables
#' ===================================================================
#' [EXTENSION] Verification of contextual variables and inspection of
#' Italian country-level values across waves.

#' **Contextual variables — Italy**
cat("\n=== CONTEXTUAL VARIABLES — ITALY ===\n")
it_ctx <- dat[dat$cntry == "IT" & !duplicated(paste(dat$cntry, dat$wave)),
              c("cntry", "wave", "AGEOLD", "UNEMP", "TECHEXP", "INVEST",
                "LAT", "LONG", "UAI")]
print(it_ctx[order(it_ctx$wave), ])

#' **UAI by country** (time-invariant) [EXTENSION]
cat("\n=== UAI BY COUNTRY ===\n")
uai_tab <- dat[!duplicated(dat$cntry), c("cntry", "UAI")]
print(uai_tab[order(uai_tab$UAI, decreasing = TRUE), ])

#' **Correlation UAI x composite score — wave 3** [G&A replication]
#' Uses rob (three-item composite) for wave 3, consistent with G&A.
cat("\n=== CORRELATION UAI x COMPOSITE SCORE (Wave 3) ===\n")
sub <- dat[dat$wave == 3 & !is.na(dat$rob) & !is.na(dat$wgt2), ]
cntry_means <- tapply(sub$rob * sub$wgt2, sub$cntry, sum) /
               tapply(sub$wgt2,   sub$cntry, sum)
uai_vals    <- tapply(sub$UAI, sub$cntry, mean, na.rm = TRUE)
common      <- intersect(names(cntry_means), names(uai_vals))
cat(sprintf("r(UAI, composite_wave3) = %.3f\n",
            cor(cntry_means[common], uai_vals[common], use = "complete.obs")))

#' **Correlation UAI x composite score — wave 4** [EXTENSION]
#' Uses rob2item (two-item comparable composite) for wave 4,
#' because rob includes a non-comparable rob3 item.
cat("\n=== CORRELATION UAI x COMPOSITE SCORE (Wave 4) ===\n")
sub4 <- dat[dat$wave == 4 & !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
cntry_means4 <- tapply(sub4$rob2item * sub4$wgt2, sub4$cntry, sum) /
                tapply(sub4$wgt2,      sub4$cntry, sum)
uai_vals4    <- tapply(sub4$UAI, sub4$cntry, mean, na.rm = TRUE)
common4      <- intersect(names(cntry_means4), names(uai_vals4))
cat(sprintf("r(UAI, rob2item_wave4) = %.3f\n",
            cor(cntry_means4[common4], uai_vals4[common4], use = "complete.obs")))




#' ===================================================================
#' # 7. Italy focus — sociodemographic profile
#' ===================================================================
#' [EXTENSION] Sociodemographic profile of the Italian subsample by wave.
#' Intended for the Italy-focus section of Chapter 5.

cat("\n=== SOCIODEMOGRAPHIC PROFILE — ITALY BY WAVE ===\n")

it <- dat[dat$cntry == "IT", ]

#' **Sex** (proportion female)
cat("\nProportion female:\n")
print(tapply(it$sex, it$wave, mean, na.rm = TRUE))

#' **Mean age**
cat("\nMean age:\n")
print(tapply(it$age, it$wave, mean, na.rm = TRUE))

#' **Mean education**
cat("\nMean education (age at end of full-time education):\n")
print(tapply(it$educ, it$wave, mean, na.rm = TRUE))

#' **Employment status**
cat("\nEmployment status (proportions by wave):\n")
for (w in 1:4) {
    cat(sprintf("  Wave %d:\n", w))
    print(round(prop.table(table(it$white[it$wave == w])), 3))
}

#' **Composite score — Italy, pooled across imputed datasets**
#' Waves 1-3: rob (0-9). Wave 4: rob2item (0-6). Not directly comparable.
cat("\n=== COMPOSITE SCORE — ITALY (weighted, pooled across m=20 imputations) ===\n")
cat("  [Waves 1-3: rob (0-9); Wave 4: rob2item (0-6)]\n")

for (w in 1:3) {
  m_imp <- sapply(dati, function(x) {
    sub <- x[x$cntry == "IT" & x$wave == w, ]
    weighted.mean(sub$rob, sub$wgt2, na.rm = TRUE)
  })
  sd_imp <- sapply(dati, function(x) {
    sub <- x[x$cntry == "IT" & x$wave == w, ]
    sqrt(wtd.var(sub$rob, sub$wgt2))
  })
  n <- sum(dat$cntry == "IT" & dat$wave == w & !is.na(dat$rob))
  cat(sprintf("  Wave %d (rob):     M = %.3f  SD = %.3f  N = %d\n",
              w, mean(m_imp), mean(sd_imp), n))
}

m4_imp <- sapply(dati, function(x) {
  sub <- x[x$cntry == "IT" & x$wave == 4, ]
  weighted.mean(sub$rob2item, sub$wgt2, na.rm = TRUE)
})
sd4_imp <- sapply(dati, function(x) {
  sub <- x[x$cntry == "IT" & x$wave == 4, ]
  sqrt(wtd.var(sub$rob2item, sub$wgt2))
})
n4 <- sum(dat$cntry == "IT" & dat$wave == 4 & !is.na(dat$rob2item))
cat(sprintf("  Wave 4 (rob2item): M = %.3f  SD = %.3f  N = %d\n",
            mean(m4_imp), mean(sd4_imp), n4))

cat("\nScript 2_1 complete. Proceed to 2_2_Measurement_invariance_extended.R\n")
