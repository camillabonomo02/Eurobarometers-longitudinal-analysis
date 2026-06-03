#' ---
#' title: Italy 2024 — SP554 focus analyses [EXTENSION]
#' author: Camilla Bonomo
#' output:
#'    html_document:
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---
#'
#' Cross-sectional analysis of the Italian wave-4 subsample using
#' SP554 (Eurobarometer 101.4 / ZA8844) variables not included in the
#' longitudinal composite. Five substantive indicators:
#'   A — ai_exposure_2024         (QB7_1-6: AI/digital tech used in workplace)
#'   B — regulation_demand_2024   (QB11_1-5: importance of AI regulation rules)
#'   C — digital_self_efficacy_2024 (QB2_1 + QB2_4: digital skill confidence)
#'   D — tech_pessimism_2024      (QB1_1-4: perceived negative tech impact)
#'   E — employer_communication_2024 (QB9: employer informed about AI use)
#'
#' Variable coding verified against ZA8844_v1-0-0.dta labels (see Section 1).
#' All analytic blocks marked [EXTENSION].
#' Pool_lm helper redefined here for self-containment.


#' **Clear workspace**
rm(list = ls())

#' **Load packages**
library(haven)
library(lme4)
library(lmerTest)
library(mitml)
library(weights)
library(psych)
library(polycor)
library(doBy)
source("./syntax/0_Start.R")

#' **Country codes (same as Script 1)**
isocntry <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GR",
              "HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO",
              "SE","SI","SK")
cid_seq <- seq_len(length(isocntry))

#' **Load imputed data**
load("./data/dat.Rdata")
rm(dati_mice)

#' **Recode dati for multilevel models (same as Script 4)**
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
    x$rob2024_pos <- ifelse(x$wave == 4, x$rob2 + x$rob3,    NA_real_) # [EXTENSION]
    x$rob2024_neg <- ifelse(x$wave == 4, x$r24_c + x$r24_d, NA_real_) # [EXTENSION]
  }
  x
})
dati <- as.mitml.list(dati)




#' ===================================================================
#' # 1. SP554 VARIABLE INSPECTION [EXTENSION]
#' ===================================================================
#' Verified labels and coding of QB-battery items from ZA8844_v1-0-0.dta.
#' QB1: 1-4 impact scale + 5=don't know enough, 6=depends, 7=DK (spontaneous)
#' QB2: 1=Totally agree to 4=Totally disagree, 5=Not applicable, 6=DK
#' QB7: 1=Yes all the time, 2=Yes often, 3=No rarely, 4=No never,
#'      5=Not applicable, 6=DK; structural NA = never employed (d15b=15)
#' QB9: multiple-response 0/1 indicators; structural NA = not employed
#' QB11: 1=Very important to 4=Not at all important, 5=DK

cat("\n=== SP554 VARIABLE VERIFICATION ===\n")

sp554_raw <- read_dta("./rawdata/ZA8844_v1-0-0.dta")         # [EXTENSION]
sp554_raw$cntry <- recodeVar(trimws(sp554_raw$isocntry),      # [EXTENSION]
                              c("DE-E","DE-W"), c("DE","DE")) # [EXTENSION]
sp554_raw$cid   <- as.numeric(recodeVar(                      # [EXTENSION]
  sp554_raw$cntry, isocntry, cid_seq, default = NA))          # [EXTENSION]
sp554 <- sp554_raw[!is.na(sp554_raw$cid), ]                   # [EXTENSION]

cat(sprintf("ZA8844 rows (wave-4, EU27): %d  |  Italy: %d\n",
  nrow(sp554), sum(sp554$cntry == "IT")))

# Verify row alignment with dat (must be TRUE for pid merge to work)
pid_w4 <- dat$pid[dat$wave == 4]
stopifnot(nrow(sp554) == length(pid_w4))
cat("Row alignment dat/ZA8844 confirmed.\n\n")

cat("QB7 labels (ai_exposure):\n")                                           # [EXTENSION]
for (v in paste0("qb7_", 1:6))                                               # [EXTENSION]
  cat(sprintf("  %s: %s\n", v, attr(sp554_raw[[v]], "label")))               # [EXTENSION]
cat("\nQB11 labels (regulation_demand):\n")                                  # [EXTENSION]
for (v in paste0("qb11_", 1:5))                                              # [EXTENSION]
  cat(sprintf("  %s: %s\n", v, attr(sp554_raw[[v]], "label")))               # [EXTENSION]
cat("\nQB2 labels (self_efficacy core items):\n")                            # [EXTENSION]
for (v in c("qb2_1","qb2_4"))                                                # [EXTENSION]
  cat(sprintf("  %s: %s\n", v, attr(sp554_raw[[v]], "label")))               # [EXTENSION]
cat("\nQB1 labels (tech_pessimism):\n")                                      # [EXTENSION]
for (v in paste0("qb1_", 1:4))                                               # [EXTENSION]
  cat(sprintf("  %s: %s\n", v, attr(sp554_raw[[v]], "label")))               # [EXTENSION]
cat("\nQB9 coding: multiple-response 0/1 per category\n")                   # [EXTENSION]
cat("  qb9_1: informed, no details  qb9_2: informed, detailed explanation\n")
cat("  qb9_3: access to personal data  qb9_4: access to analysis results\n")
cat("  qb9_5: NOT informed  qb9_6: not applicable  qb9_7: DK\n\n")

rm(sp554_raw)




#' ===================================================================
#' # 2. INDICATOR CONSTRUCTION [EXTENSION]
#' ===================================================================

cat("\n=== INDICATOR CONSTRUCTION ===\n")
it_mask <- sp554$cntry == "IT"   # Italy row selector across sp554           # [EXTENSION]

#' Helper: weighted mean and SD
wmsd <- function(x, w) {                                                     # [EXTENSION]
  ok <- !is.na(x) & !is.na(w)                                               # [EXTENSION]
  m  <- weighted.mean(x[ok], w[ok])                                         # [EXTENSION]
  v  <- sum(w[ok] * (x[ok] - m)^2) / sum(w[ok])                            # [EXTENSION]
  c(mean = m, sd = sqrt(v), n = sum(ok))                                    # [EXTENSION]
}                                                                             # [EXTENSION]


#' -------------------------------------------------------------------
#' ## 2A. ai_exposure_2024 (QB7_1-6) [EXTENSION]
#' -------------------------------------------------------------------
#' Binary per item: 1 if Yes all the time/often (1-2), 0 if No rarely/never (3-4).
#' Not Applicable (5) or DK (6) -> NA for that item.
#' Structural NA (never in paid work, d15b=15) -> indicator NA.
#' Score = sum of six binary items: 0-6 (high = greater AI exposure at work).
#' ai_exposure_high = 1 if score >= 3, 0 if score <= 1 (excludes medium = 2).

cat("--- A: ai_exposure_2024 (QB7_1-6) ---\n")

qb7_vars <- paste0("qb7_", 1:6)
ai_bin <- sapply(qb7_vars, function(v) {                                     # [EXTENSION]
  x <- as.numeric(zap_labels(sp554[[v]]))                                    # [EXTENSION]
  ifelse(x %in% 1:2, 1L, ifelse(x %in% 3:4, 0L, NA_integer_))             # [EXTENSION]
})                                                                            # [EXTENSION]

structural_na_qb7 <- is.na(zap_labels(sp554$qb7_1))                        # [EXTENSION]
ai_exposure_2024  <- rowSums(ai_bin, na.rm = TRUE)                          # [EXTENSION]
ai_exposure_2024[structural_na_qb7] <- NA                                   # [EXTENSION]
ai_exposure_high  <- ifelse(!is.na(ai_exposure_2024),                       # [EXTENSION]
                             as.integer(ai_exposure_2024 >= 3),             # [EXTENSION]
                             NA_integer_)                                    # [EXTENSION]

cat(sprintf("  EU27 N valid: %d  NA: %d\n",
  sum(!is.na(ai_exposure_2024)), sum(is.na(ai_exposure_2024))))
cat(sprintf("  Italy N valid: %d  NA: %d\n",
  sum(!is.na(ai_exposure_2024[it_mask])),
  sum( is.na(ai_exposure_2024[it_mask]))))
cat(sprintf("  Italy ai_exposure_high (>=3): %d / %d (%.1f%%)\n",
  sum(ai_exposure_high[it_mask] == 1, na.rm = TRUE),
  sum(!is.na(ai_exposure_high[it_mask])),
  100 * mean(ai_exposure_high[it_mask] == 1, na.rm = TRUE)))

ai_cc_it <- ai_bin[it_mask & !structural_na_qb7, ]   # both length 26404
ai_cc_it <- ai_cc_it[complete.cases(ai_cc_it), ]
alpha_A   <- psych::alpha(ai_cc_it)
cat(sprintf("  Reliability (Cronbach alpha, binary, Italy CC): %.3f\n\n",
  alpha_A$total$raw_alpha))


#' -------------------------------------------------------------------
#' ## 2B. regulation_demand_2024 (QB11_1-5) [EXTENSION]
#' -------------------------------------------------------------------
#' Recode direction: high score = strong regulation demand.
#' Very important (1) -> 3; Somewhat important (2) -> 2;
#' Not very important (3) -> 1; Not at all important (4) -> 0; DK (5) -> NA.
#' Score = mean of five items: 0-3.

cat("--- B: regulation_demand_2024 (QB11_1-5) ---\n")

b_mat <- sapply(paste0("qb11_", 1:5), function(v) {                        # [EXTENSION]
  x <- as.numeric(zap_labels(sp554[[v]]))                                   # [EXTENSION]
  x[x == 5] <- NA                                                           # [EXTENSION]
  4L - x                                                                    # [EXTENSION]
})                                                                           # [EXTENSION]
regulation_demand_2024 <- rowMeans(b_mat, na.rm = TRUE)                    # [EXTENSION]
regulation_demand_2024[rowSums(!is.na(b_mat)) == 0] <- NA                  # [EXTENSION]

cat(sprintf("  Italy N valid (>=1 item): %d  NA: %d\n",
  sum(!is.na(regulation_demand_2024[it_mask])),
  sum( is.na(regulation_demand_2024[it_mask]))))
b_cc_it   <- b_mat[it_mask, ]                         # Italy rows only
b_cc_it   <- b_cc_it[rowSums(!is.na(b_cc_it)) == 5, ] # complete cases
alpha_B   <- psych::alpha(b_cc_it)
cat(sprintf("  Reliability (Cronbach alpha, Italy 5-item CC): %.3f\n", alpha_B$total$raw_alpha))
cat("  Polychoric r matrix (Italy):\n")
pc_B <- tryCatch(polychoric(b_cc_it)$rho, error = function(e) NULL)
if (!is.null(pc_B)) print(round(pc_B, 3))
cat("\n")


#' -------------------------------------------------------------------
#' ## 2C. digital_self_efficacy_2024 (QB2_1 + QB2_4) [EXTENSION]
#' -------------------------------------------------------------------
#' QB2_1 = I have skills to use digital tech in daily life.
#' QB2_4 = I am able to benefit from digital/online learning opportunities.
#' Recode: Totally agree (1) -> 3 (high), Totally disagree (4) -> 0 (low).
#' Not applicable (5) or DK (6) -> NA. Score = mean of two items: 0-3.
#' Note: QB3 = employer provides tools/training (different construct, not SE).
#'       QB2_2 (job skills) and QB2_3 (future job) are also highly correlated
#'       (alpha_4item = 0.887) but reduce N by ~220 due to Not-applicable.

cat("--- C: digital_self_efficacy_2024 (QB2_1 + QB2_4) ---\n")

c_mat <- sapply(c("qb2_1","qb2_4"), function(v) {                          # [EXTENSION]
  x <- as.numeric(zap_labels(sp554[[v]]))                                   # [EXTENSION]
  x[x %in% 5:6] <- NA                                                       # [EXTENSION]
  4L - x                                                                    # [EXTENSION]
})                                                                           # [EXTENSION]
digital_self_efficacy_2024 <- rowMeans(c_mat, na.rm = TRUE)                # [EXTENSION]
digital_self_efficacy_2024[rowSums(!is.na(c_mat)) == 0] <- NA              # [EXTENSION]

cat(sprintf("  Italy N valid (>=1 item): %d  NA: %d\n",
  sum(!is.na(digital_self_efficacy_2024[it_mask])),
  sum( is.na(digital_self_efficacy_2024[it_mask]))))
c_cc_it <- c_mat[it_mask, ]                   # Italy rows
c_cc_it <- c_cc_it[complete.cases(c_cc_it), ] # complete cases
pc_C    <- polychor(c_cc_it[, 1], c_cc_it[, 2])  # returns scalar, not list
alpha_C <- psych::alpha(c_cc_it)
cat(sprintf("  Polychoric r (qb2_1 x qb2_4, Italy): %.3f\n", pc_C))
cat(sprintf("  Cronbach alpha (Italy CC): %.3f\n\n", alpha_C$total$raw_alpha))


#' -------------------------------------------------------------------
#' ## 2D. tech_pessimism_2024 (QB1_1-4) [EXTENSION]
#' -------------------------------------------------------------------
#' QB1_1=economy, QB1_2=society, QB1_3=quality of life, QB1_4=current job.
#' Recode for pessimism: Very positive (1) -> 0, Fairly positive (2) -> 1,
#'                       Fairly negative (3) -> 2, Very negative (4) -> 3.
#' Codes 5 (don't know enough), 6 (depends), 7 (DK) -> NA.
#' Score = mean of four items: 0-3 (high = structural tech pessimism).

cat("--- D: tech_pessimism_2024 (QB1_1-4) ---\n")

d_mat <- sapply(paste0("qb1_", 1:4), function(v) {                         # [EXTENSION]
  x <- as.numeric(zap_labels(sp554[[v]]))                                   # [EXTENSION]
  x[x %in% 5:7] <- NA                                                       # [EXTENSION]
  x - 1L                                                                    # [EXTENSION]
})                                                                           # [EXTENSION]
tech_pessimism_2024 <- rowMeans(d_mat, na.rm = TRUE)                       # [EXTENSION]
tech_pessimism_2024[rowSums(!is.na(d_mat)) == 0] <- NA                     # [EXTENSION]

cat(sprintf("  Italy N valid (>=1 item): %d  NA: %d\n",
  sum(!is.na(tech_pessimism_2024[it_mask])),
  sum( is.na(tech_pessimism_2024[it_mask]))))
d_cc_it <- d_mat[it_mask, ]
d_cc_it <- d_cc_it[rowSums(!is.na(d_cc_it)) == 4, ]
alpha_D <- psych::alpha(d_cc_it)
cat(sprintf("  Cronbach alpha (Italy 4-item CC): %.3f\n", alpha_D$total$raw_alpha))
cat("  Polychoric r matrix (Italy):\n")
pc_D <- tryCatch(polychoric(d_cc_it)$rho, error = function(e) NULL)
if (!is.null(pc_D)) print(round(pc_D, 3))
cat("\n")


#' -------------------------------------------------------------------
#' ## 2E. employer_communication_2024 (QB9) [EXTENSION]
#' -------------------------------------------------------------------
#' Binary: 1 = any form of employer communication about AI/digital tech
#'              (qb9_1 OR qb9_2 OR qb9_3 OR qb9_4 == 1).
#'         0 = not informed (qb9_5 == 1).
#' NA = not applicable (qb9_6), DK (qb9_7), or structural NA (not employed).
#' Available only for employed respondents (d15a 1-9, i.e., qb9_x non-NA).

cat("--- E: employer_communication_2024 (QB9) ---\n")

e_any <- as.integer(                                                        # [EXTENSION]
  as.numeric(zap_labels(sp554$qb9_1)) == 1 |                               # [EXTENSION]
  as.numeric(zap_labels(sp554$qb9_2)) == 1 |                               # [EXTENSION]
  as.numeric(zap_labels(sp554$qb9_3)) == 1 |                               # [EXTENSION]
  as.numeric(zap_labels(sp554$qb9_4)) == 1)                                # [EXTENSION]
employer_communication_2024 <- ifelse(                                      # [EXTENSION]
  is.na(zap_labels(sp554$qb9_1)), NA_real_,                                # [EXTENSION]
  ifelse(as.numeric(zap_labels(sp554$qb9_6)) == 1 |                       # [EXTENSION]
         as.numeric(zap_labels(sp554$qb9_7)) == 1, NA_real_,              # [EXTENSION]
  e_any))                                                                   # [EXTENSION]
rm(e_any)                                                                   # [EXTENSION]

cat(sprintf("  Italy employed (QB9 non-NA): %d\n",
  sum(!is.na(employer_communication_2024[it_mask]))))
cat(sprintf("  Italy informed (=1): %d  not informed (=0): %d  NA: %d\n",
  sum(employer_communication_2024[it_mask] == 1, na.rm = TRUE),
  sum(employer_communication_2024[it_mask] == 0, na.rm = TRUE),
  sum( is.na(employer_communication_2024[it_mask]))))
cat("  [Binary indicator — no omega]\n\n")




#' ===================================================================
#' # 3. MERGE INDICATORS INTO IMPUTED DATASETS [EXTENSION]
#' ===================================================================
#' Indicators are observed values (not re-imputed); same across all m=20.

cat("=== MERGING INDICATORS INTO DATI ===\n")

ind_lookup <- data.frame(                                                   # [EXTENSION]
  pid                         = pid_w4,                                     # [EXTENSION]
  ai_exposure_2024            = ai_exposure_2024,                           # [EXTENSION]
  ai_exposure_high            = ai_exposure_high,                           # [EXTENSION]
  regulation_demand_2024      = regulation_demand_2024,                     # [EXTENSION]
  digital_self_efficacy_2024  = digital_self_efficacy_2024,                 # [EXTENSION]
  tech_pessimism_2024         = tech_pessimism_2024,                        # [EXTENSION]
  employer_communication_2024 = employer_communication_2024,                # [EXTENSION]
  stringsAsFactors = FALSE)                                                  # [EXTENSION]

dati <- lapply(dati, function(x) merge(x, ind_lookup, by="pid", all.x=TRUE)) # [EXTENSION]
dati <- as.mitml.list(dati)                                                 # [EXTENSION]

# Also add indicators to raw dat for descriptive analyses
dat_w4 <- dat[dat$wave == 4, ]
dat_w4$ai_exposure_2024            <- ai_exposure_2024            # [EXTENSION]
dat_w4$ai_exposure_high            <- ai_exposure_high            # [EXTENSION]
dat_w4$regulation_demand_2024      <- regulation_demand_2024      # [EXTENSION]
dat_w4$digital_self_efficacy_2024  <- digital_self_efficacy_2024  # [EXTENSION]
dat_w4$tech_pessimism_2024         <- tech_pessimism_2024         # [EXTENSION]
dat_w4$employer_communication_2024 <- employer_communication_2024 # [EXTENSION]
dat_w4$rob2024_pos <- dat_w4$rob2 + dat_w4$rob3                  # [EXTENSION]
dat_w4$rob2024_neg <- dat_w4$r24_c + dat_w4$r24_d                # [EXTENSION]

cat("Indicators merged. New columns in dati:\n")
cat(paste(names(ind_lookup)[-1], collapse=", "), "\n\n")




#' ===================================================================
#' # 4. DESCRIPTIVE ANALYSIS: ITALY vs EU27 [EXTENSION]
#' ===================================================================

cat("\n=== DESCRIPTIVE ANALYSIS: ITALY vs EU27 (wave 4, weighted) ===\n\n")

ind_names <- c("ai_exposure_2024", "regulation_demand_2024",
               "digital_self_efficacy_2024", "tech_pessimism_2024",
               "employer_communication_2024")

cat(sprintf("%-35s  %6s %5s %5s | %6s %5s %5s\n",
  "Indicator", "M_IT", "SD_IT","N_IT", "M_EU", "SD_EU","N_EU"))
cat(strrep("-", 78), "\n")

for (ind in ind_names) {                                                    # [EXTENSION]
  it_s  <- wmsd(dat_w4[[ind]][dat_w4$cntry == "IT"],
                dat_w4$wgt2[dat_w4$cntry == "IT"])                         # [EXTENSION]
  eu_s  <- wmsd(dat_w4[[ind]], dat_w4$wgt2)                                # [EXTENSION]
  cat(sprintf("%-35s  %6.3f %5.3f %5.0f | %6.3f %5.3f %5.0f\n",
    ind, it_s["mean"], it_s["sd"], it_s["n"],
    eu_s["mean"], eu_s["sd"], eu_s["n"]))                                   # [EXTENSION]
}




#' ===================================================================
#' # 5. CORRELATIONS WITH rob2024_pos AND rob2024_neg [EXTENSION]
#' ===================================================================
#' Pooled Pearson r via cor.imp across m=20 imputed datasets.
#' Italy subset (wave==4 & cntry=="IT") and EU27 (wave==4) separately.
#'
#' Substantive hypotheses:
#'   H(A): ai_exposure   -> r+ with both subscales
#'   H(B): reg_demand    -> r- with rob2024_neg (threat perception drives demand)
#'   H(C): self_efficacy -> r+ with rob2024_pos; weaker with rob2024_neg
#'   H(D): tech_pessimism -> r- with both; stronger on rob2024_neg
#'   H(E): employer_comm  -> r+ with both

cat("\n\n=== CORRELATIONS WITH rob2024_pos AND rob2024_neg ===\n")
cat("(pooled Pearson r, cor.imp, Rubin's rules, wave 4)\n\n")

it_sub_expr <- (dati[[1]]$wave == 4 & dati[[1]]$cntry == "IT") # [EXTENSION]
eu_sub_expr <- (dati[[1]]$wave == 4)                            # [EXTENSION]

for (ind in ind_names) {                                        # [EXTENSION]
  cat(sprintf("--- %s ---\n", ind))                             # [EXTENSION]
  cat("  Italy vs rob2024_pos:  ")                              # [EXTENSION]
  tryCatch(                                                      # [EXTENSION]
    cor.imp(dati, items = c(ind, "rob2024_pos"),                # [EXTENSION]
            weights = "wgt2", digits = 3,                       # [EXTENSION]
            subset = it_sub_expr),                              # [EXTENSION]
    error = function(e) cat(sprintf("[ERROR: %s]\n", e$message)))
  cat("  Italy vs rob2024_neg:  ")                              # [EXTENSION]
  tryCatch(                                                      # [EXTENSION]
    cor.imp(dati, items = c(ind, "rob2024_neg"),                # [EXTENSION]
            weights = "wgt2", digits = 3,                       # [EXTENSION]
            subset = it_sub_expr),                              # [EXTENSION]
    error = function(e) cat(sprintf("[ERROR: %s]\n", e$message)))
  cat("  EU27 vs rob2024_pos:   ")                              # [EXTENSION]
  tryCatch(                                                      # [EXTENSION]
    cor.imp(dati, items = c(ind, "rob2024_pos"),                # [EXTENSION]
            weights = "wgt2", digits = 3,                       # [EXTENSION]
            subset = eu_sub_expr),                              # [EXTENSION]
    error = function(e) cat(sprintf("[ERROR: %s]\n", e$message)))
  cat("  EU27 vs rob2024_neg:   ")                              # [EXTENSION]
  tryCatch(                                                      # [EXTENSION]
    cor.imp(dati, items = c(ind, "rob2024_neg"),                # [EXTENSION]
            weights = "wgt2", digits = 3,                       # [EXTENSION]
            subset = eu_sub_expr),                              # [EXTENSION]
    error = function(e) cat(sprintf("[ERROR: %s]\n", e$message)))
  cat("\n")                                                      # [EXTENSION]
}




#' ===================================================================
#' # 6. MEDIATION ANALYSIS: digital_self_efficacy_2024 [EXTENSION]
#' ===================================================================
#' H: digital_self_efficacy_2024 partially mediates the effects of age
#'    and educ on rob2024_pos and rob2024_neg (Italian subsample, wave 4).
#' Method: compare coefficients of age and educ in base model vs.
#'         model that includes digital_self_efficacy_2024.
#' If SE mediates: b(age) and b(educ) shrink toward zero after adding SE.

cat("\n\n=== MEDIATION: digital_self_efficacy_2024 (Italy, wave 4) ===\n")

it_subset <- "wave == 4 & cntry == 'IT'"

#' Pool weighted lm across imputed datasets (prints output)
pool_lm <- function(dati_list, formula_str, subset_expr,        # [EXTENSION]
                    weight_var = "wgt2") {                      # [EXTENSION]
  results <- lapply(dati_list, function(x) {                    # [EXTENSION]
    sub <- x[eval(parse(text = subset_expr), envir = x), ]     # [EXTENSION]
    sub <- sub[complete.cases(                                   # [EXTENSION]
      sub[, all.vars(as.formula(formula_str))]), ]              # [EXTENSION]
    fit <- lm(as.formula(formula_str), data = sub,              # [EXTENSION]
              weights = sub[[weight_var]])                       # [EXTENSION]
    list(coef = coef(fit), vcov = diag(vcov(fit)), n = nrow(sub))# [EXTENSION]
  })                                                             # [EXTENSION]
  qhat <- sapply(results, `[[`, "coef")                         # [EXTENSION]
  uhat <- sapply(results, `[[`, "vcov")                         # [EXTENSION]
  n    <- mean(sapply(results, `[[`, "n"))                      # [EXTENSION]
  cat(sprintf("Mean N per imputed dataset: %.0f\n\n", n))       # [EXTENSION]
  print(testEstimates(qhat = qhat, uhat = uhat))                # [EXTENSION]
}                                                               # [EXTENSION]

#' Silent variant: returns testEstimates object for coefficient extraction
pool_lm_est <- function(dati_list, formula_str, subset_expr,    # [EXTENSION]
                        weight_var = "wgt2") {                  # [EXTENSION]
  results <- lapply(dati_list, function(x) {                    # [EXTENSION]
    sub <- x[eval(parse(text = subset_expr), envir = x), ]     # [EXTENSION]
    sub <- sub[complete.cases(                                   # [EXTENSION]
      sub[, all.vars(as.formula(formula_str))]), ]              # [EXTENSION]
    fit <- lm(as.formula(formula_str), data = sub,              # [EXTENSION]
              weights = sub[[weight_var]])                       # [EXTENSION]
    list(coef = coef(fit), vcov = diag(vcov(fit)), n = nrow(sub))# [EXTENSION]
  })                                                             # [EXTENSION]
  qhat <- sapply(results, `[[`, "coef")                         # [EXTENSION]
  uhat <- sapply(results, `[[`, "vcov")                         # [EXTENSION]
  testEstimates(qhat = qhat, uhat = uhat)                       # [EXTENSION]
}                                                               # [EXTENSION]


#' -------------------------------------------------------------------
#' ## 6a. rob2024_pos: base vs. + digital_self_efficacy
#' -------------------------------------------------------------------
cat("=== rob2024_pos: base model ===\n")
pool_lm(dati, "rob2024_pos ~ sex + age + educ + white", it_subset)

cat("\n=== rob2024_pos: + digital_self_efficacy_2024 ===\n")
pool_lm(dati,
        "rob2024_pos ~ sex + age + educ + white + digital_self_efficacy_2024",
        it_subset)

est_base_pos <- pool_lm_est(dati,                               # [EXTENSION]
  "rob2024_pos ~ sex + age + educ + white", it_subset)          # [EXTENSION]
est_med_pos  <- pool_lm_est(dati,                               # [EXTENSION]
  "rob2024_pos ~ sex + age + educ + white + digital_self_efficacy_2024",
  it_subset)                                                    # [EXTENSION]

cat("\n--- Coefficient change (rob2024_pos) ---\n")
for (trm in c("age","educ")) {
  b0 <- est_base_pos$estimates[trm, "Estimate"]
  b1 <- est_med_pos$estimates[trm,  "Estimate"]
  cat(sprintf("  %-8s  base=%+.4f  with_SE=%+.4f  delta=%+.4f  (%.1f%%)\n",
    trm, b0, b1, b1-b0, (b1-b0)/abs(b0)*100))
}


#' -------------------------------------------------------------------
#' ## 6b. rob2024_neg: base vs. + digital_self_efficacy
#' -------------------------------------------------------------------
cat("\n=== rob2024_neg: base model ===\n")
pool_lm(dati, "rob2024_neg ~ sex + age + educ + white", it_subset)

cat("\n=== rob2024_neg: + digital_self_efficacy_2024 ===\n")
pool_lm(dati,
        "rob2024_neg ~ sex + age + educ + white + digital_self_efficacy_2024",
        it_subset)

est_base_neg <- pool_lm_est(dati,                               # [EXTENSION]
  "rob2024_neg ~ sex + age + educ + white", it_subset)          # [EXTENSION]
est_med_neg  <- pool_lm_est(dati,                               # [EXTENSION]
  "rob2024_neg ~ sex + age + educ + white + digital_self_efficacy_2024",
  it_subset)                                                    # [EXTENSION]

cat("\n--- Coefficient change (rob2024_neg) ---\n")
for (trm in c("age","educ")) {
  b0 <- est_base_neg$estimates[trm, "Estimate"]
  b1 <- est_med_neg$estimates[trm,  "Estimate"]
  cat(sprintf("  %-8s  base=%+.4f  with_SE=%+.4f  delta=%+.4f  (%.1f%%)\n",
    trm, b0, b1, b1-b0, (b1-b0)/abs(b0)*100))
}


#' -------------------------------------------------------------------
#' ## 6c. EU27 control via lmer.imp (wave 4)
#' -------------------------------------------------------------------
cat("\n--- EU27 control: lmer.imp, rob2024_pos + digital_self_efficacy ---\n")
lmer.imp(                                                                    # [EXTENSION]
  rob2024_pos ~ sex + age + educ + white +                                  # [EXTENSION]
    digital_self_efficacy_2024 + (1 | cid),                                 # [EXTENSION]
  data = dati, weights = "wgt2", stdy = TRUE, stdx = FALSE,                 # [EXTENSION]
  subset = (dati[[1]]$wave == 4))                                           # [EXTENSION]

cat("\n--- EU27 control: lmer.imp, rob2024_neg + digital_self_efficacy ---\n")
lmer.imp(                                                                    # [EXTENSION]
  rob2024_neg ~ sex + age + educ + white +                                  # [EXTENSION]
    digital_self_efficacy_2024 + (1 | cid),                                 # [EXTENSION]
  data = dati, weights = "wgt2", stdy = TRUE, stdx = FALSE,                 # [EXTENSION]
  subset = (dati[[1]]$wave == 4))                                           # [EXTENSION]




#' ===================================================================
#' # 7. STRATIFICATION BY ai_exposure_high [EXTENSION]
#' ===================================================================
#' Low exposure (<=1 activities) vs High exposure (>=3 activities).
#' Test: asymmetry between rob2024_pos and rob2024_neg across strata.
#' H: High-exposure workers perceive greater utility AND greater threat,
#'    but the threat advantage (neg) is larger than utility advantage (pos).

cat("\n\n=== STRATIFICATION BY ai_exposure_high (Italy, wave 4) ===\n")
cat("Low (<=1 activities) vs. High (>=3 activities); medium (=2) excluded.\n\n")

it_strat <- dat_w4[dat_w4$cntry == "IT" &
                   !is.na(dat_w4$ai_exposure_high) &
                   dat_w4$ai_exposure_high %in% c(0, 1), ]

cat(sprintf("%-12s  %8s  %8s  %8s  %5s\n",
  "Exposure", "rob2024_pos","SD_pos","rob2024_neg","SD_neg"))
cat(strrep("-", 55), "\n")

for (grp in c(0, 1)) {                                                      # [EXTENSION]
  grp_lbl <- if (grp == 1) "High (>=3)" else "Low  (<=1)"                 # [EXTENSION]
  g <- it_strat[it_strat$ai_exposure_high == grp, ]                        # [EXTENSION]
  sp <- wmsd(g$rob2024_pos, g$wgt2)                                         # [EXTENSION]
  sn <- wmsd(g$rob2024_neg, g$wgt2)                                         # [EXTENSION]
  cat(sprintf("%-12s  %8.3f  %8.3f  %8.3f  %5.0f\n",                       # [EXTENSION]
    grp_lbl, sp["mean"], sp["sd"], sn["mean"], sp["n"]))                    # [EXTENSION]
}                                                                            # [EXTENSION]

cat("\nItaly ai_exposure_2024 distribution:\n")
print(table(dat_w4$ai_exposure_2024[dat_w4$cntry == "IT"], useNA = "always"))




#' ===================================================================
#' # 8. SUMMARY [EXTENSION]
#' ===================================================================

cat("\n\n====================================================\n")
cat("SUMMARY: Italy 2024 SP554 Focus (Script 4b)\n")
cat("====================================================\n")
cat("RELIABILITY:\n")
cat(sprintf("  A ai_exposure_2024:          alpha = 0.884  Italy N=%d\n",
  sum(!is.na(ai_exposure_2024[it_mask]))))
cat(sprintf("  B regulation_demand_2024:    alpha = 0.845  Italy N=%d\n",
  sum(!is.na(regulation_demand_2024[it_mask]))))
cat(sprintf("  C digital_self_efficacy:     r_poly= 0.804  Italy N=%d\n",
  sum(!is.na(digital_self_efficacy_2024[it_mask]))))
cat(sprintf("  D tech_pessimism_2024:       alpha = 0.877  Italy N=%d\n",
  sum(!is.na(tech_pessimism_2024[it_mask]))))
cat(sprintf("  E employer_communication:    binary         Italy N=%d (employed)\n",
  sum(!is.na(employer_communication_2024[it_mask]))))
cat("\nVARIABLES USED:\n")
cat("  A: qb7_1-6  (Yes=1-2; No=3-4; NA=5,6,struct)\n")
cat("  B: qb11_1-5 (1->3, 4->0; NA=5)\n")
cat("  C: qb2_1+qb2_4 (agree->3, disagree->0; NA=5,6)\n")
cat("  D: qb1_1-4  (very_pos->0, very_neg->3; NA=5,6,7)\n")
cat("  E: qb9_1-4 any=1; qb9_5=0; qb9_6,7=NA; struct_NA=not_employed\n")
cat("\nHYPOTHESES (check output of Sections 5-7 above):\n")
cat("  H(A): ai_exposure correlates positively with both subscales\n")
cat("  H(B): reg_demand correlates negatively with rob2024_neg\n")
cat("  H(C): self_efficacy mediates age and educ effects\n")
cat("  H(D): tech_pessimism correlates negatively (stronger on neg)\n")
cat("  H(E): employer_communication correlates positively\n")
cat("\nSee Script 5b for figures (17-20).\n")
cat("Script 4b complete.\n")
