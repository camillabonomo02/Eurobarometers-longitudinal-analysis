#' ---
#' title: Hofstede explorations — DEPRECATED, not in final analysis
#' note: Retained for transparency. NOT sourced by the main pipeline.
#'       See ./syntax/archive/README.md for rationale.
#' ---
#'
#' This file collects all Hofstede-related inferential code that was
#' removed from the active pipeline during the structural cleanup of
#' the repository. It can be sourced independently to reproduce the
#' exploratory results.
#'
#' Sources (original file — original line range):
#'   4_Predictors_extended.R lines  95-129  : UAI/Hofstede z-score standardisation
#'   4_Predictors_extended.R lines 267-381  : Models B2, B2_multi, B3
#'   4_Predictors_extended.R lines 385-555  : A2 vs. B2 comparison + Italian residuals
#'   4_Predictors_extended.R lines 800-933  : Section 10 one-at-a-time, VIF, joint
#'   5_Plots_extended.R      lines 617-711  : Figure 5 (UAI scatter)
#'   5_Plots_extended.R      lines 716-822  : Figure 6 (random effects A2 vs B2)


# ===================================================================
# SELF-CONTAINED SETUP
# ===================================================================

library(lme4)
library(lmerTest)
library(mitml)
library(weights)
library(ggplot2)
library(ggrepel)
source("./syntax/0_Start.R")

load("./data/dat.Rdata")
rm(dati_mice)

dati <- within(dati, {
  white <- as.factor(white)
  sex   <- as.factor(sex)
  wave  <- as.factor(wave)
  age   <- scale(age,  scale = FALSE) / 10
  educ  <- scale(educ, scale = FALSE)
})

dati <- lapply(dati, function(x) {
  x$rob2item <- x$rob1 + x$rob2
  if ("r24_c" %in% names(x)) {
    x$rob2024     <- ifelse(x$wave == 4,
                            x$rob2 + x$rob3 + x$r24_c + x$r24_d,
                            NA_real_)
    x$rob2024_pos <- ifelse(x$wave == 4, x$rob2 + x$rob3,    NA_real_)
    x$rob2024_neg <- ifelse(x$wave == 4, x$r24_c + x$r24_d, NA_real_)
  }
  x
})
dati <- as.mitml.list(dati)


# ===================================================================
# UAI AND HOFSTEDE Z-SCORES
# (from 4_Predictors_extended.R lines 95-129)
# ===================================================================

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


# ===================================================================
# SINGLE-DATASET PREP FOR L2-VARIANCE AND RESIDUAL ANALYSES
# (mirrors 4_Predictors_extended.R lines 401-407)
# ===================================================================

d1 <- dati[[1]]
d1$wave_num <- as.numeric(as.character(d1$wave))
d1_123 <- d1[d1$wave_num %in% 1:3, ]

d1_123$wgt2_norm <- d1_123$wgt2 / mean(d1_123$wgt2, na.rm = TRUE)
d1$wgt2_norm     <- d1$wgt2     / mean(d1$wgt2,     na.rm = TRUE)


# ===================================================================
# MODEL B2: A2 + UAI (waves 1-3)
# (from 4_Predictors_extended.R lines 282-296)
# ===================================================================

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


# ===================================================================
# MODEL B2: A2 + UAI (waves 1-4)
# (from 4_Predictors_extended.R lines 302-315)
# ===================================================================

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


# ===================================================================
# MODEL B2_MULTI: A2 + ALL HOFSTEDE DIMENSIONS (waves 1-3)
# (from 4_Predictors_extended.R lines 324-338)
# ===================================================================

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


# ===================================================================
# MODEL B2_MULTI: A2 + ALL HOFSTEDE DIMENSIONS (waves 1-4)
# (from 4_Predictors_extended.R lines 340-353)
# ===================================================================

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


# ===================================================================
# MODEL B3: CROSS-LEVEL INTERACTIONS
# (from 4_Predictors_extended.R lines 366-381)
# ===================================================================

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


# ===================================================================
# A2 vs. B2 COMPARISON: L2 VARIANCE EXPLAINED
# (from 4_Predictors_extended.R lines 389-555)
# ===================================================================

cat("\n\n=== A2 vs. B2 COMPARISON: L2 VARIANCE EXPLAINED ===\n")
cat("Note: AIC/BIC = Inf in lme4 with large weighted samples — known overflow.\n")
cat("Variance reduction (pseudo-R2 at country level) used as alternative.\n")
cat("Residual analysis uses first imputed dataset (m=1).\n\n")

#' -------------------------------------------------------------------
#' ## Variance reduction — waves 1-3
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
#' ## Italian residuals — A2 vs. B2 (waves 1-3)
#' -------------------------------------------------------------------
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
#' ## Italian residuals — waves 1-4
#' -------------------------------------------------------------------
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
#' ## Variance reduction — A2 vs. B2_multi (waves 1-3)
#' -------------------------------------------------------------------
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


# ===================================================================
# SECTION 10: HOFSTEDE DIMENSIONS — ONE-AT-A-TIME AND JOINT
# (from 4_Predictors_extended.R lines 800-933)
# ===================================================================

cat("\n\n=== SECTION 10: HOFSTEDE DIMENSIONS — ONE-AT-A-TIME AND JOINT ===\n")
cat("[EXTENSION] Waves 1-4, rob2item. Each dimension added to Model A2.\n\n")

# Helper: extract Italian random intercept from a fitted lmer
.cid_it <- unique(d1$cid[d1$cntry == "IT"])
.it_re <- function(fit) {
  re <- data.frame(cid = as.integer(rownames(ranef(fit)$cid)),
                   re  = ranef(fit)$cid[, 1])
  re$re[re$cid == .cid_it]
}

# A2 baseline: Italian residual without any Hofstede
.m_A2_base <- lmer(
  rob2item ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG + (1 | cid),
  data = d1, weights = wgt2_norm, REML = FALSE,
  control = lmerControl(optimizer = "nloptwrap"))
.re_base <- .it_re(.m_A2_base)
cat(sprintf("A2 baseline — Italy residual (m=1): %+.3f\n\n", .re_base))

#' -------------------------------------------------------------------
#' ## 10a. One-at-a-time models
#' -------------------------------------------------------------------
.hof_z <- c("UAI_z","PDI_z","IDV_z","MAS_z","LTO_z","IVR_z")

.summ <- data.frame(
  dim     = character(), est_std = numeric(),
  p       = numeric(),   it_re   = numeric(),
  d_re    = numeric(),   stringsAsFactors = FALSE)

for (.z in .hof_z) {
  cat(sprintf("--- A2 + %s (waves 1-4, rob2item) ---\n", .z))
  .frm <- as.formula(paste0(
    "rob2item ~ wave + sex + age + educ + white + ",
    "AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG + ",
    .z, " + (1 | cid)"))
  .res <- lmer.imp(.frm, data = dati, weights = "wgt2",
                   stdy = TRUE, stdx = FALSE,
                   control = lmerControl(optimizer = "nloptwrap"))
  .m1  <- lmer(.frm, data = d1, weights = wgt2_norm, REML = FALSE,
               control = lmerControl(optimizer = "nloptwrap"))
  .re_z <- .it_re(.m1)
  cat(sprintf("  Italy residual (m=1): %+.3f  delta: %+.3f\n\n",
              .re_z, .re_z - .re_base))
  .ests <- .res$est$estimates
  .summ <- rbind(.summ, data.frame(
    dim     = .z,
    est_std = unname(.res$std[.z]),
    p       = unname(.ests[.z, ncol(.ests)]),
    it_re   = .re_z,
    d_re    = .re_z - .re_base,
    stringsAsFactors = FALSE))
}

cat("=== Summary: one-at-a-time Hofstede, rob2item waves 1-4 ===\n")
cat("  est_std: b/SD(Y); p: pooled; d_re: Italy residual change\n\n")
.summ_print <- .summ[, c("dim","est_std","p","it_re","d_re")]
.summ_print[, -1] <- round(.summ_print[, -1], 3)
print(.summ_print)
rm(.summ_print)
cat("\n  Negative d_re = dimension absorbs Italy's scepticism\n\n")

#' -------------------------------------------------------------------
#' ## 10b. VIF check: collinearity among Hofstede dimensions
#' -------------------------------------------------------------------
cat("--- VIF: six Hofstede z-scores at the country level (N = 27) ---\n")

.cntry_hof <- unique(d1[!is.na(d1$cid), c("cid", .hof_z)])
.cntry_hof <- .cntry_hof[!duplicated(.cntry_hof$cid), ]

.vif_vals <- setNames(
  sapply(.hof_z, function(dv) {
    .oth <- paste(setdiff(.hof_z, dv), collapse = " + ")
    .r2  <- summary(lm(as.formula(paste(dv, "~", .oth)),
                       data = .cntry_hof))$r.squared
    1 / (1 - .r2)
  }), .hof_z)

cat("\nVIF (manual, country-level auxiliary regressions):\n")
print(round(.vif_vals, 2))

.high_vif <- names(.vif_vals[.vif_vals > 5])
if (length(.high_vif) > 0) {
  cat(sprintf("\n  WARNING: VIF > 5 for: %s\n",
              paste(.high_vif, collapse = ", ")))
  cat("  Coefficients for these dimensions in the joint model are\n")
  cat("  unreliable. Interpret joint model with caution; rely on\n")
  cat("  one-at-a-time models for inference (Section 10a).\n\n")
} else {
  cat("\n  All VIF <= 5; joint model interpretable with caution.\n\n")
}

#' -------------------------------------------------------------------
#' ## 10c. Joint model: Italian residual only
#' -------------------------------------------------------------------
cat("--- Joint model: Italian residual (m=1, first imputed dataset) ---\n")
if (length(.high_vif) > 0)
  cat(sprintf("  [CAUTION: VIF > 5 for %s]\n",
              paste(.high_vif, collapse = ", ")))

.m_joint <- lmer(
  rob2item ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
    UAI_z + PDI_z + IDV_z + MAS_z + LTO_z + IVR_z + (1 | cid),
  data = d1, weights = wgt2_norm, REML = FALSE,
  control = lmerControl(optimizer = "nloptwrap"))
.re_joint <- .it_re(.m_joint)
cat(sprintf("\n  Italy residual: %+.3f  (A2 baseline: %+.3f  delta: %+.3f)\n",
            .re_joint, .re_base, .re_joint - .re_base))

rm(.cid_it, .it_re, .m_A2_base, .re_base, .hof_z, .z, .frm,
   .res, .m1, .re_z, .ests, .summ, .cntry_hof, .vif_vals,
   .high_vif, .m_joint, .re_joint)


# ===================================================================
# FIGURE 5: UAI SCATTER BY COUNTRY
# (from 5_Plots_extended.R lines 617-711)
# ===================================================================
#' rob2item used for both panels (waves 3 and 4).
#' geom_smooth uses inherit.aes = FALSE to fit ONE line over ALL countries.

# Colour palette helpers (replicated from 5_Plots setup)
col_italy   <- "#D55E00"
col_eu      <- "#0072B2"
col_neutral <- "#999999"
pal_div <- colorRampPalette(c("#b2182b", "#ef8a62", "#fddbc7",
                              "#f7f7f7",
                              "#d1e5f0", "#67a9cf", "#2166ac"))
theme_academic <- function(base_size = 12) {
  ggplot2::theme_bw(base_size = base_size) %+replace%
    ggplot2::theme(
      panel.grid.minor    = ggplot2::element_blank(),
      panel.grid.major    = ggplot2::element_line(colour = "grey88", linewidth = 0.3),
      strip.background    = ggplot2::element_rect(fill = "grey94", colour = "grey70", linewidth = 0.5),
      strip.text          = ggplot2::element_text(size = base_size, face = "bold", margin = ggplot2::margin(4, 4, 4, 4)),
      legend.background   = ggplot2::element_blank(),
      legend.key          = ggplot2::element_blank(),
      legend.title        = ggplot2::element_text(size = base_size - 1, face = "bold"),
      legend.text         = ggplot2::element_text(size = base_size - 1),
      plot.title          = ggplot2::element_text(size = base_size + 1, face = "bold", hjust = 0, margin = ggplot2::margin(b = 3)),
      plot.subtitle       = ggplot2::element_text(size = base_size - 1, colour = "grey40", hjust = 0, margin = ggplot2::margin(b = 5)),
      plot.caption        = ggplot2::element_text(size = base_size - 2, colour = "grey50", hjust = 0, margin = ggplot2::margin(t = 4)),
      axis.text           = ggplot2::element_text(colour = "grey30", size = base_size - 1),
      axis.title          = ggplot2::element_text(size = base_size, face = "bold"),
      plot.margin         = ggplot2::margin(10, 12, 10, 10)
    )
}

cat("\n=== FIGURE 5: UAI vs. COMPOSITE SCORE (rob2item) ===\n")

# Use the raw dat (not imputed) — rob2item already in dat from Script 1
dat$rob2item <- dat$rob1 + dat$rob2

scatter_data <- do.call(rbind, lapply(unique(dat$cntry), function(cc) {
  uai_val <- dat$UAI[dat$cntry == cc][1]
  m3 <- {
    sub <- dat[dat$cntry == cc & dat$wave == 3 &
                 !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
    if (nrow(sub) > 0) weighted.mean(sub$rob2item, sub$wgt2) else NA
  }
  m4 <- {
    sub <- dat[dat$cntry == cc & dat$wave == 4 &
                 !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
    if (nrow(sub) > 0) weighted.mean(sub$rob2item, sub$wgt2) else NA
  }
  data.frame(cntry = cc, UAI = uai_val, score_w3 = m3, score_w4 = m4)
}))
scatter_data$is_italy <- scatter_data$cntry == "IT"

r_2017 <- cor(scatter_data$UAI, scatter_data$score_w3, use = "complete.obs")
r_2024 <- cor(scatter_data$UAI, scatter_data$score_w4, use = "complete.obs")
cat(sprintf("  r(UAI, rob2item_2017) = %.3f\n", r_2017))
cat(sprintf("  r(UAI, rob2item_2024) = %.3f\n", r_2024))

scatter_long <- rbind(
  data.frame(cntry    = scatter_data$cntry,
             UAI      = scatter_data$UAI,
             score    = scatter_data$score_w3,
             wave     = "2017",
             is_italy = scatter_data$is_italy),
  data.frame(cntry    = scatter_data$cntry,
             UAI      = scatter_data$UAI,
             score    = scatter_data$score_w4,
             wave     = "2024",
             is_italy = scatter_data$is_italy)
)
scatter_long <- scatter_long[!is.na(scatter_long$score) &
                               !is.na(scatter_long$UAI), ]
scatter_long$wave <- factor(scatter_long$wave, levels = c("2017", "2024"))

y_max <- max(scatter_long$score, na.rm = TRUE)
y_min <- min(scatter_long$score, na.rm = TRUE)
uai_max <- max(scatter_long$UAI, na.rm = TRUE)

r_labels <- data.frame(
  wave  = factor(c("2017", "2024"), levels = c("2017", "2024")),
  label = c(sprintf("italic(r) == %.3f", r_2017),
            sprintf("italic(r) == %.3f", r_2024)),
  UAI   = uai_max,
  score = y_max - 0.05 * (y_max - y_min)
)

p5 <- ggplot(scatter_long, aes(x = UAI, y = score)) +
  geom_smooth(inherit.aes = FALSE,
              mapping      = aes(x = UAI, y = score),
              method       = "lm", se = TRUE,
              colour       = "grey40", fill = "grey80",
              linewidth    = 0.8, linetype = "dashed") +
  geom_point(aes(colour = is_italy, size = is_italy)) +
  geom_text_repel(aes(label = cntry, colour = is_italy),
                  size = 2.8, max.overlaps = 20,
                  segment.size = 0.2, segment.color = "grey60") +
  geom_text(data        = r_labels,
            aes(x = UAI, y = score, label = label),
            inherit.aes = FALSE,
            parse       = TRUE,
            hjust = 1, vjust = 1,
            size = 4, fontface = "italic", colour = "grey25") +
  scale_colour_manual(values = c("FALSE" = col_neutral, "TRUE" = col_italy),
                      labels = c("EU countries", "Italy"), name = NULL) +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4), guide = "none") +
  facet_wrap(~ wave, scales = "fixed",
             labeller = labeller(wave = c("2017" = "2017 (rob2item)",
                                          "2024" = "2024 (rob2item)"))) +
  labs(title    = "Uncertainty Avoidance Index and robot attitudes by EU country",
       subtitle = "Each point = one country; dashed line = OLS fit (95% CI) over all 27 countries.",
       x        = "Hofstede Uncertainty Avoidance Index (UAI)",
       y        = "Weighted mean score (rob2item, 0-6)",
       caption  = "Fixed y-axis enables direct comparison of association strength across waves.") +
  theme_academic(base_size = 12) +
  theme(legend.position = "bottom")

dir.create("./plots", showWarnings = FALSE)
ggsave("./plots/Figure_5_UAI_scatter.png", p5,
       width = 12, height = 6, dpi = 150)
cat("Saved: Figure_5_UAI_scatter.png\n")
rm(scatter_data, scatter_long, r_labels, r_2017, r_2024, p5,
   y_max, y_min, uai_max)


# ===================================================================
# FIGURE 6: COUNTRY RANDOM EFFECTS WITH ITALY HIGHLIGHTED
# (from 5_Plots_extended.R lines 716-822)
# ===================================================================
#' Models A2 and B2 fitted on rob2item (waves 1-4). Random effects (BLUPs)
#' averaged across all m=20 imputed datasets.

cat("\n=== FIGURE 6: COUNTRY RANDOM EFFECTS ===\n")

load("./data/dat.Rdata")
rm(dati_mice)
dati <- lapply(dati, function(x) {
  x$rob2item <- x$rob1 + x$rob2
  x
})
dati <- as.mitml.list(dati)

dati <- within(dati, {
  white <- as.factor(white)
  sex   <- as.factor(sex)
  wave  <- as.factor(wave)
  age   <- scale(age,  scale = FALSE) / 10
  educ  <- scale(educ, scale = FALSE)
})

uai_mean <- mean(unique(dati[[1]][, c("cid", "UAI")])$UAI, na.rm = TRUE)
uai_sd   <- sd(unique(dati[[1]][,  c("cid", "UAI")])$UAI,  na.rm = TRUE)
dati <- lapply(dati, function(x) { x$UAI_z <- (x$UAI - uai_mean) / uai_sd; x })
dati <- as.mitml.list(dati)

m_A2_list <- lapply(dati, function(x) {
  lmer(rob2item ~ wave + sex + age + educ + white +
         AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
         (1 | cid),
       data = x, weights = x$wgt2,
       control = lmerControl(optimizer = "nloptwrap"))
})

m_B2_list <- lapply(dati, function(x) {
  lmer(rob2item ~ wave + sex + age + educ + white +
         AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG + UAI_z +
         (1 | cid),
       data = x, weights = x$wgt2,
       control = lmerControl(optimizer = "nloptwrap"))
})

re_A2 <- Reduce("+", lapply(m_A2_list, function(m) ranef(m)$cid)) /
  length(m_A2_list)
re_B2 <- Reduce("+", lapply(m_B2_list, function(m) ranef(m)$cid)) /
  length(m_B2_list)

re_A2$cid <- as.integer(rownames(re_A2))
re_B2$cid <- as.integer(rownames(re_B2))
cid_cntry <- unique(dati[[1]][, c("cid", "cntry")])
re_A2 <- merge(re_A2, cid_cntry, by = "cid")
re_B2 <- merge(re_B2, cid_cntry, by = "cid")
names(re_A2)[2] <- "re_A2"
names(re_B2)[2] <- "re_B2"

re_long <- rbind(
  data.frame(cntry = re_A2$cntry, re = re_A2$re_A2, model = "A2 (without UAI)"),
  data.frame(cntry = re_B2$cntry, re = re_B2$re_B2, model = "B2 (with UAI)")
)
re_long$is_italy <- re_long$cntry == "IT"

ord <- re_A2$cntry[order(re_A2$re_A2)]
re_long$cntry <- factor(re_long$cntry, levels = ord)

it_pos  <- which(levels(re_long$cntry) == "IT")
re_range <- range(re_long$re, na.rm = TRUE)

p6 <- ggplot(re_long, aes(x = cntry, y = re)) +
  geom_hline(yintercept = 0, colour = "grey40", linetype = "dashed",
             linewidth = 0.5) +
  geom_line(aes(group = cntry, colour = is_italy),
            linewidth = 0.6, alpha = 0.5) +
  geom_point(aes(colour = is_italy, shape = model), size = 2.5) +
  annotate("text", x = it_pos, y = re_range[1] - 0.05,
           label = "Italy", colour = col_italy,
           size = 3.5, fontface = "bold") +
  annotate("text", x = length(levels(re_long$cntry)) - 1,
           y = re_range[2] + 0.03,
           label = "Above prediction", colour = col_eu,
           size = 2.8, fontface = "italic", hjust = 0.5) +
  annotate("text", x = 2, y = re_range[1] + 0.03,
           label = "Below prediction", colour = col_italy,
           size = 2.8, fontface = "italic", hjust = 0.5) +
  scale_colour_manual(values = c("FALSE" = col_neutral, "TRUE" = col_italy),
                      labels = c("Other EU countries", "Italy"), name = NULL) +
  scale_shape_manual(values = c("A2 (without UAI)" = 16, "B2 (with UAI)" = 17),
                     name = "Model") +
  coord_cartesian(clip = "off") +
  labs(title    = "Country random effects: Model A2 vs. B2 (waves 1-4, rob2item)",
       subtitle = "Lines connect A2 to B2 per country. Negative = below structural prediction.",
       x        = "Country (ordered by A2 random intercept)",
       y        = "Random intercept",
       caption  = "Models fitted on rob2item across m = 20 imputed datasets; BLUPs averaged.") +
  theme_academic(base_size = 11) +
  theme(axis.text.x    = element_text(angle = 45, hjust = 1, size = 8.5),
        legend.position = "bottom",
        legend.box      = "horizontal",
        plot.margin     = margin(10, 12, 15, 10))

ggsave("./plots/Figure_6_random_effects.png", p6,
       width = 13, height = 6.5, dpi = 150)
cat("Saved: Figure_6_random_effects.png\n")
rm(m_A2_list, m_B2_list, re_A2, re_B2, re_long, ord, p6,
   uai_mean, uai_sd, cid_cntry, it_pos, re_range)


cat("\nHofstede_explorations.R complete.\n")
cat("These analyses are NOT part of the final thesis pipeline.\n")
cat("See ./syntax/archive/README.md for rationale.\n")
