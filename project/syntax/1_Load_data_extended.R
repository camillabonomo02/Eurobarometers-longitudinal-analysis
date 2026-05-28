#' ---
#' title: "Prepare data for analyses — Replication and extension of Gnambs & Appel (2019)"
#' author: "Camilla Bonomo"
#' output:
#'    html_document:
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---
#'
#' This script adapts the original Gnambs & Appel (2019) code to:
#'   1. Add wave 4 (EB 101.4 / SP EB 554, 2024)
#'   2. Add Hofstede UAI as a country-level predictor [EXTENSION]
#'   3. Exclude the United Kingdom (absent from wave 4 post-Brexit)
#'   4. Prepare the Italian subsample for the Italy focus [EXTENSION]
#'
#' Comments marked [G&A] indicate code replicated from the original.
#' Comments marked [EXTENSION] indicate original contributions of this thesis.
#'
#' Methodological note on imputation of composite scores:
#'   The composites rob (rob1+rob2+rob3) and rob2item (rob1+rob2) are
#'   treated as passive variables in the MICE algorithm (method = "~ I(...)").
#'   Passive imputation ensures internal consistency: composites are
#'   recomputed algebraically from their imputed components after each
#'   iteration, rather than being imputed independently (which would
#'   violate the deterministic relationship between items and composites).
#'   Both composites are excluded from the predictor matrix (column = 0)
#'   to prevent circularity.


#' **Clear workspace**
rm(list = ls())

#' **Load packages**
library(haven)       # [G&A] for read_dta
library(dplyr)       # [G&A]
library(doBy)        # [G&A] for recodeVar
library(mice)        # [G&A] for multiple imputation
library(mitml)       # [G&A] for pooling results
library(readxl)      # [EXTENSION] for contextual data


# [G&A] Suppress automatic string-to-factor conversion
options(stringsAsFactors = FALSE)




#' ===================================================================
#' # CONFIGURATION — FILE PATHS
#' ===================================================================
#' Update these paths to match the local directory structure.

# Directory containing the raw .dta files downloaded from GESIS
RAWDATA_DIR <- "./rawdata"

# Directory containing country-level contextual data
CONTEXTDATA_DIR <- "./contextdata"

# Output directory
DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)




#' ===================================================================
#' # 1. Load Eurobarometer microdata
#' ===================================================================

#' **Wave 1: February/March 2012** [G&A]
dat1 <- zap_labels(read_dta(file.path(RAWDATA_DIR, "ZA5597_v3-0-0.dta")))

#' **Wave 2: December 2014** [G&A]
dat2 <- zap_labels(read_dta(file.path(RAWDATA_DIR, "ZA5933_v6-0-0.dta")))

#' **Wave 3: March 2017** [G&A]
dat3 <- zap_labels(read_dta(file.path(RAWDATA_DIR, "ZA6861_v2-0-0.dta")))

#' **Wave 4: April-May 2024** [EXTENSION]
dat4 <- zap_labels(read_dta(file.path(RAWDATA_DIR, "ZA8844_v1-0-0.dta")))




#' ===================================================================
#' # 2. Load and prepare country-level contextual data
#' ===================================================================

#' **Country codes** [G&A modified: UK removed, HR added]
#' [EXTENSION] The United Kingdom is excluded for consistency across all waves,
#' as it is absent from wave 4 following withdrawal from the European Union.
isocntry <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES",
              "FI", "FR", "GR", "HR", "HU", "IE", "IT", "LT",
              "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI",
              "SK")
cid_seq <- seq_len(length(isocntry))

#' -------------------------------------------------------------------
#' ## 2a. World Bank indicators [G&A]
#' -------------------------------------------------------------------
#' Five indicators are extracted per country and survey year:
#'   SP.POP.65UP.TO.ZS  -> AGEOLD  (population aged 65+, %)
#'   SL.UEM.TOTL.ZS     -> UNEMP   (unemployment rate)
#'   TX.VAL.TECH.MF.ZS  -> TECHEXP (high-technology exports, % of manufactured exports)
#'   GB.XPD.RSDV.GD.ZS  -> RESEXP  (R&D expenditure, % of GDP)
#'   SP.POP.SCIE.RD.P6  -> RESEAR  (researchers in R&D, per million inhabitants)
#'
#' INVEST = mean of z-scores of RESEAR and RESEXP (replicates G&A).

wb <- read.csv(file.path(CONTEXTDATA_DIR, "worldbank.csv"),
               stringsAsFactors = FALSE)

# Remove rows with missing or malformed Series.Code
wb <- wb[!is.na(wb$Series.Code) & nchar(wb$Series.Code) > 3, ]

# ISO3 to ISO2 country code mapping
iso3to2 <- c(AUT="AT", BEL="BE", BGR="BG", HRV="HR", CYP="CY",
             CZE="CZ", DNK="DK", EST="EE", FIN="FI", FRA="FR",
             DEU="DE", GRC="GR", HUN="HU", IRL="IE", ITA="IT",
             LVA="LV", LTU="LT", LUX="LU", MLT="MT", NLD="NL",
             POL="PL", PRT="PT", ROU="RO", SVK="SK", SVN="SI",
             ESP="ES", SWE="SE")
wb$cntry <- iso3to2[wb$Country.Code]

# Map World Bank Series.Code to variable names
ind_map <- c(SP.POP.65UP.TO.ZS = "AGEOLD",
             SL.UEM.TOTL.ZS    = "UNEMP",
             TX.VAL.TECH.MF.ZS = "TECHEXP",
             GB.XPD.RSDV.GD.ZS = "RESEXP",
             SP.POP.SCIE.RD.P6 = "RESEAR")
wb$varname <- ind_map[wb$Series.Code]
wb <- wb[!is.na(wb$varname) & !is.na(wb$cntry), ]

# Year columns to be extracted
year_cols   <- c("X2012..YR2012.", "X2014..YR2014.",
                 "X2017..YR2017.", "X2022..YR2022.", "X2024..YR2024.")
year_labels <- c(12, 14, 17, 22, 24)

# Build country-level dataset in wide format (one row per country)
cntry_data <- data.frame(cntry = isocntry, stringsAsFactors = FALSE)

for (i in seq_along(year_cols)) {
    yr <- year_labels[i]
    for (var in unique(wb$varname)) {
        col_name <- paste0(var, yr)
        vals <- wb[wb$varname == var, c("cntry", year_cols[i])]
        vals[[year_cols[i]]] <- suppressWarnings(as.numeric(vals[[year_cols[i]]]))
        names(vals) <- c("cntry", col_name)
        cntry_data <- merge(cntry_data, vals, by = "cntry", all.x = TRUE)
    }
}

#' **2024 R&D data unavailable** — World Bank has not yet published
#' R&D indicators for 2024. The 2022 values are used as a proxy,
#' consistent with the assumption of relative stability in national
#' R&D investment levels across adjacent years.

if ("RESEXP24" %in% names(cntry_data) && all(is.na(cntry_data$RESEXP24))) {
    message("Note: RESEXP 2024 missing — substituting 2022 values as proxy")
    cntry_data$RESEXP24 <- cntry_data$RESEXP22
}
if ("RESEAR24" %in% names(cntry_data) && all(is.na(cntry_data$RESEAR24))) {
    message("Note: RESEAR 2024 missing — substituting 2022 values as proxy")
    cntry_data$RESEAR24 <- cntry_data$RESEAR22
}

#' **Construct INVEST composite** [G&A — exact replication]
#' INVEST = mean of within-wave z-scores of RESEAR and RESEXP.

has_resear <- any(grepl("RESEAR", names(cntry_data)))

if (has_resear) {
    message("RESEAR available — constructing INVEST as in G&A (mean z-score of RESEAR + RESEXP)")
    cntry_data$INVEST12 <- rowMeans(apply(cntry_data[, c("RESEAR12", "RESEXP12")], 2, scale))
    cntry_data$INVEST14 <- rowMeans(apply(cntry_data[, c("RESEAR14", "RESEXP14")], 2, scale))
    cntry_data$INVEST17 <- rowMeans(apply(cntry_data[, c("RESEAR17", "RESEXP17")], 2, scale))
    cntry_data$INVEST24 <- rowMeans(apply(cntry_data[, c("RESEAR24", "RESEXP24")], 2, scale))
} else {
    message("RESEAR not found — INVEST approximated as z-score of RESEXP (proxy; document in appendix)")
    cntry_data$INVEST12 <- scale(cntry_data$RESEXP12)[, 1]
    cntry_data$INVEST14 <- scale(cntry_data$RESEXP14)[, 1]
    cntry_data$INVEST17 <- scale(cntry_data$RESEXP17)[, 1]
    cntry_data$INVEST24 <- scale(cntry_data$RESEXP24)[, 1]
}

#' -------------------------------------------------------------------
#' ## 2b. Geographic coordinates [G&A]
#' -------------------------------------------------------------------
geo <- data.frame(
    cntry = isocntry,
    LAT = c(47.5, 50.5, 42.7, 35.1, 49.8, 51.2, 56.3, 58.6, 40.5,
            61.9, 46.2, 39.1, 45.1, 47.2, 53.1, 41.9, 55.2,
            49.8, 56.9, 35.9, 52.1, 51.9, 39.4, 45.9, 60.1, 46.2, 48.7),
    LONG = c(14.6, 4.5, 25.5, 33.4, 15.5, 10.5, 9.5, 25.0, -3.7,
             25.7, 2.2, 21.8, 15.2, 19.5, -8.0, 12.6, 23.9,
             6.1, 24.1, 14.5, 5.3, 19.1, -8.2, 25.0, 18.6, 14.8, 19.7),
    stringsAsFactors = FALSE
)
cntry_data <- merge(cntry_data, geo, by = "cntry", all.x = TRUE)

#' -------------------------------------------------------------------
#' ## 2c. Hofstede cultural dimensions [EXTENSION]
#' -------------------------------------------------------------------
#' All six Hofstede dimensions are treated as time-invariant L2 predictors:
#'   PDI  — Power Distance Index
#'   IDV  — Individualism
#'   MAS  — Masculinity
#'   UAI  — Uncertainty Avoidance Index
#'   LTO  — Long-Term Orientation
#'   IVR  — Indulgence vs. Restraint
#'
#' Source: Hofstede (2010) / hofstede-insights.com.
#' hofstede_eu27.csv is a pre-cleaned file keyed by ISO2 country code,
#' covering all 27 EU countries in the sample.
#' Cyprus (CY) is absent from the Hofstede (2010) print edition; all six
#' dimension scores (PDI=57, IDV=35, MAS=57, UAI=65, LTO=45, IVR=70) are
#' sourced from the Hofstede Insights online tool (hofstede-insights.com).

hof <- read.csv(file.path(CONTEXTDATA_DIR, "hofstede_eu27.csv"),
                stringsAsFactors = FALSE)
names(hof) <- c("cntry", "PDI", "IDV", "MAS", "UAI", "LTO", "IVR")

cntry_data <- merge(cntry_data, hof, by = "cntry", all.x = TRUE)

#' **Numeric country identifier** [G&A]
cntry_data$cid <- recodeVar(cntry_data$cntry, isocntry, cid_seq,
                             default = NA)
cntry_data$cid <- as.numeric(cntry_data$cid)

#' **Reshape to long format** [G&A; extended to 4 waves]
cntry_long <- reshape(cntry_data,
                      varying = list(
                          c("AGEOLD12",  "AGEOLD14",  "AGEOLD17",  "AGEOLD24"),
                          c("TECHEXP12", "TECHEXP14", "TECHEXP17", "TECHEXP24"),
                          c("INVEST12",  "INVEST14",  "INVEST17",  "INVEST24"),
                          c("UNEMP12",   "UNEMP14",   "UNEMP17",   "UNEMP24")),
                      v.names  = c("AGEOLD", "TECHEXP", "INVEST", "UNEMP"),
                      timevar  = "wave",
                      idvar    = "cid",
                      drop     = c("RESEXP12", "RESEXP14", "RESEXP17", "RESEXP24",
                                   "RESEAR12", "RESEAR14", "RESEAR17", "RESEAR24",
                                   "RESEAR22", "RESEXP22",
                                   "AGEOLD22", "TECHEXP22", "UNEMP22"),
                      direction = "long")
# UAI, LAT, and LONG are time-invariant and are already present in long format




#' ===================================================================
#' # 3. Recode individual-level variables
#' ===================================================================

#' -------------------------------------------------------------------
#' ## Wave 1: 2012 [G&A]
#' -------------------------------------------------------------------
dat1$wave <- 1

dat1$cntry <- recodeVar(trimws(dat1$isocntry),
                        c("DE-E", "DE-W", "GB-GBN", "GB-NIR"),
                        c("DE",   "DE",   "GB",     "GB"))
dat1$cid <- as.numeric(recodeVar(dat1$cntry, isocntry, cid_seq, default = NA))

# Attitude items: recoded so that higher values indicate more positive attitudes [G&A]
dat1$rob1  <- recodeVar(dat1$qa4,   1:4, 3:0, default = NA)
dat1$rob2  <- recodeVar(dat1$qa5_1, 1:4, 3:0, default = NA)
dat1$rob3  <- recodeVar(dat1$qa5_3, 1:4, 3:0, default = NA)
dat1$feel1 <- recodeVar(dat1$qa8_1, 1:10, 0:9, default = NA)
dat1$feel2 <- recodeVar(dat1$qa8_3, 1:10, 0:9, default = NA)
dat1$feel3 <- NA  # not available in wave 1
dat1$feel4 <- NA  # not available in wave 1

# Sociodemographic variables [G&A]
dat1$married <- recodeVar(dat1$d7r2, 1:5,  1:5,  default = NA)
dat1$educ    <- recodeVar(dat1$d8r1, 1:11, c(1:9, 9, 0), default = NA)
dat1$sex     <- recodeVar(dat1$d10,  1:2,  0:1,  default = NA)
dat1$age     <- recodeVar(dat1$d11,  15:99, 15:99, default = NA)
dat1$empl    <- recodeVar(dat1$d15a, 1:18, 1:18, default = NA)

# Survey weights [G&A]
dat1$wgt1 <- dat1$w1
dat1$wgt2 <- dat1$w22


#' -------------------------------------------------------------------
#' ## Wave 2: 2014 [G&A]
#' -------------------------------------------------------------------
dat2$wave <- 2

dat2$cntry <- recodeVar(trimws(dat2$isocntry),
                        c("DE-E", "DE-W", "GB-GBN", "GB-NIR"),
                        c("DE",   "DE",   "GB",     "GB"))
dat2$cid <- as.numeric(recodeVar(dat2$cntry, isocntry, cid_seq, default = NA))

dat2$rob1  <- recodeVar(dat2$qa4,   1:4, 3:0, default = NA)
dat2$rob2  <- recodeVar(dat2$qa6_1, 1:4, 3:0, default = NA)
dat2$rob3  <- recodeVar(dat2$qa6_3, 1:4, 3:0, default = NA)
dat2$feel1 <- recodeVar(dat2$qa7_1, 1:10, 0:9, default = NA)
dat2$feel2 <- recodeVar(dat2$qa7_2, 1:10, 0:9, default = NA)
dat2$feel3 <- recodeVar(dat2$qa7_4, 1:10, 0:9, default = NA)
dat2$feel4 <- recodeVar(dat2$qa8_1, 1:10, 0:9, default = NA)

dat2$married <- recodeVar(dat2$d7r2, 1:5,  1:5,  default = NA)
dat2$educ    <- recodeVar(dat2$d8r1, 1:11, c(1:9, 9, 0), default = NA)
dat2$sex     <- recodeVar(dat2$d10,  1:2,  0:1,  default = NA)
dat2$age     <- recodeVar(dat2$d11,  15:99, 15:99, default = NA)
dat2$empl    <- recodeVar(dat2$d15a, 1:18, 1:18, default = NA)

dat2$wgt1 <- dat2$w1
dat2$wgt2 <- dat2$w22


#' -------------------------------------------------------------------
#' ## Wave 3: 2017 [G&A]
#' -------------------------------------------------------------------
dat3$wave <- 3

dat3$cntry <- recodeVar(trimws(dat3$isocntry),
                        c("DE-E", "DE-W", "GB-GBN", "GB-NIR"),
                        c("DE",   "DE",   "GB",     "GB"))
dat3$cid <- as.numeric(recodeVar(dat3$cntry, isocntry, cid_seq, default = NA))

dat3$rob1  <- recodeVar(dat3$qd10,    1:4, 3:0, default = NA)
dat3$rob2  <- recodeVar(dat3$qd12_2,  1:4, 3:0, default = NA)  # "helps people"
dat3$rob3  <- recodeVar(dat3$qd12_4,  1:4, 3:0, default = NA)  # "does hard/dangerous jobs"
dat3$feel1 <- recodeVar(dat3$qd13_1, 1:10, 0:9, default = NA)
dat3$feel2 <- recodeVar(dat3$qd13_2, 1:10, 0:9, default = NA)
dat3$feel3 <- recodeVar(dat3$qd13_3, 1:10, 0:9, default = NA)
dat3$feel4 <- recodeVar(dat3$qd13_5, 1:10, 0:9, default = NA)

dat3$married <- recodeVar(dat3$d7r2, 1:5,  1:5,  default = NA)
dat3$educ    <- recodeVar(dat3$d8r1, 1:11, c(1:9, 9, 0), default = NA)
dat3$sex     <- recodeVar(dat3$d10,  1:2,  0:1,  default = NA)
dat3$age     <- recodeVar(dat3$d11,  15:99, 15:99, default = NA)
dat3$empl    <- recodeVar(dat3$d15a, 1:18, 1:18, default = NA)

dat3$wgt1 <- dat3$w1
dat3$wgt2 <- dat3$w22


#' -------------------------------------------------------------------
#' ## Wave 4: 2024 [EXTENSION]
#' -------------------------------------------------------------------
#' Key differences relative to waves 1-3:
#'   - Survey refers to "robots and AI" rather than "robots" alone
#'   - rob3 wording changed: "boring/repetitive jobs" (vs. "hard/dangerous")
#'     — renders rob3 non-comparable across all four waves
#'   - United Kingdom absent (post-Brexit)
#'   - Gender variable includes a non-binary option (N = 49), coded NA
#'   - feel items are not available with a comparable 1-10 scale

dat4$wave <- 4

dat4$cntry <- recodeVar(trimws(dat4$isocntry),
                        c("DE-E", "DE-W"),
                        c("DE",   "DE"))
dat4$cid <- as.numeric(recodeVar(dat4$cntry, isocntry, cid_seq, default = NA))

#' Attitude items [EXTENSION]
#' rob1 = qb5:  general appraisal of robots/AI in the workplace (1=very positively, 4=very negatively)
#' rob2 = qb6_2: robots/AI help people (1=totally agree, 4=totally disagree)
#' rob3 = qb6_4: AI necessary for boring/repetitive tasks — WORDING CHANGED from waves 1-3
dat4$rob1 <- recodeVar(dat4$qb5,   1:4, 3:0, default = NA)
dat4$rob2 <- recodeVar(dat4$qb6_2, 1:4, 3:0, default = NA)
dat4$rob3 <- recodeVar(dat4$qb6_4, 1:4, 3:0, default = NA)

# feel items: not available as a comparable 1-10 scale in wave 4
dat4$feel1 <- NA
dat4$feel2 <- NA
dat4$feel3 <- NA
dat4$feel4 <- NA

dat4$educ    <- recodeVar(dat4$d8r1, 1:11, c(1:9, 9, 0), default = NA)
dat4$married <- recodeVar(dat4$d7r,  1:5,  1:5, default = NA)
dat4$sex     <- recodeVar(dat4$d10,  1:2,  0:1, default = NA)
dat4$age     <- suppressWarnings(as.numeric(dat4$d11))
dat4$age[dat4$age < 15 | dat4$age > 99] <- NA
dat4$empl    <- recodeVar(dat4$d15a, 1:18, 1:18, default = NA)

dat4$wgt1 <- dat4$w1
dat4$wgt2 <- dat4$w22

#' Additional items for the Italy focus [EXTENSION]
dat4$steal_jobs   <- recodeVar(dat4$qb6_5, 1:4, 3:0, default = NA)
dat4$careful_mgmt <- recodeVar(dat4$qb6_3, 1:4, 3:0, default = NA)




#' ===================================================================
#' # 4. Merge individual and country-level data
#' ===================================================================

#' [G&A; extended to wave 4, UK excluded, HR correction applied]
items <- c("cid", "wave",
           paste0("rob", 1:3), paste0("feel", 1:4),
           "married", "age", "sex", "educ", "empl",
           "wgt1", "wgt2")

dat <- suppressWarnings(
    bind_rows(dat1[, items], dat2[, items]) %>%
    bind_rows(dat3[, items]) %>%
    bind_rows(dat4[, items]) %>%
    left_join(cntry_long, by = c("cid", "wave")) %>%
    filter(!is.na(cid)) %>%            # removes UK (cid = NA after post-Brexit exclusion)
    filter(!(cntry == "HR" & wave == 1))  # [G&A] Croatia not included in wave 1
)
rm(dat1, dat2, dat3, dat4, items)




#' ===================================================================
#' # 5. Derive composite scores and auxiliary variables
#' ===================================================================

#' **Employment category** [G&A]
#' 1 = white-collar; 2 = blue-collar; 3 = non-employed
dat$white <- recodeVar(dat$empl,
                       1:18,
                       c(3, 3, 3, 3, 2, 2, 1, 2,
                         rep(1, 6), 2, 2, 2, 2),
                       default = NA)

#' **Three-item composite (waves 1-3 only)** [G&A]
#' rob = rob1 + rob2 + rob3, range 0-9.
#' Not comparable across all four waves because rob3 wording changed in 2024.
dat$rob <- rowSums(dat[, paste0("rob", 1:3)])

#' **Two-item composite (waves 1-4, comparable)** [EXTENSION]
#' rob2item = rob1 + rob2, range 0-6.
#' Used for all longitudinal analyses extending to wave 4, including
#' cross-wave multilevel models, trajectory plots, and the Italy focus.
dat$rob2item <- rowSums(dat[, paste0("rob", 1:2)])

#' **Unique respondent identifier** [G&A]
dat$pid <- seq_len(nrow(dat))

#' **Sample report**
cat("\n=== SAMPLE OVERVIEW ===\n")
cat("Total observations:", nrow(dat), "\n")
cat("By wave:\n")
print(table(dat$wave))
cat("\nBy country (wave 4):\n")
print(sort(table(dat$cntry[dat$wave == 4])))
cat("\nN Italy by wave:\n")
print(table(dat$wave[dat$cntry == "IT"]))




#' ===================================================================
#' # 6. Multiple imputation [G&A; extended with rob2item]
#' ===================================================================
#' G&A use MICE with m = 20 imputations and the CART method.
#'
#' Passive imputation for composite scores:
#'   Both 'rob' and 'rob2item' are designated as passive variables
#'   (method = "~ I(...)"). Passive imputation recomputes each composite
#'   algebraically after each MICE iteration, maintaining the definitional
#'   relationship between items and their sums. This avoids the inconsistency
#'   that arises when a composite is imputed independently of its components.
#'   Both composites are also excluded from the predictor matrix (column = 0)
#'   to prevent circularity in the imputation model.

dat_rdata_path <- file.path(DATA_DIR, "dat.Rdata")

if (file.exists(dat_rdata_path)) {

  cat("\ndat.Rdata found — loading existing imputed data, skipping MICE.\n")
  load(dat_rdata_path)
  cat("  dat:        ", nrow(dat), "rows (raw data)\n")
  cat("  dati:       ", length(dati), "imputed datasets\n")
  cat("  dati_mice:  mice object\n")

} else {

  #' **Select variables for imputation** [G&A; rob2item added]
  #' Passive composites (rob, rob2item) are placed last in items_imp
  #' to ensure that the component items are visited before the composites
  #' within each MICE iteration.
  items_imp <- c("pid", "cid",
                 paste0("rob", 1:3),
                 paste0("feel", 1:4),
                 "sex", "age", "educ",
                 "white", "wave", "wgt1", "wgt2",
                 "rob", "rob2item")   # passive composites — visited last

  d <- dat[, items_imp]
  d$white <- as.factor(d$white)
  d$sex   <- as.factor(d$sex)

  #' **Initialise imputation** [G&A]
  ini <- mice(d[, items_imp], maxit = 0, seed = 2352398)
  cat("\nProportion missing per variable:\n")
  print(round(ini$nmis / nrow(d), 3))

  #' **Predictor matrix** [G&A; composites excluded as predictors]
  #' Administrative/design variables (pid, cid, wave, weights) and
  #' derived composites (rob, rob2item) are excluded from the predictor
  #' matrix: they are neither imputed actively nor used as predictors.
  ini$predictorMatrix[c("pid", "cid", "wave", "wgt1", "wgt2",
                         "rob", "rob2item"), items_imp] <- 0
  ini$predictorMatrix[items_imp, c("pid", "cid", "wave", "wgt1", "wgt2",
                                     "rob", "rob2item")] <- 0

  #' **Imputation methods** [G&A; passive method for composites]
  ini$method[c(paste0("rob", 1:3), paste0("feel", 1:4), "educ")] <- "cart"
  ini$method["rob"]      <- "~ I(rob1 + rob2 + rob3)"  # passive: sum of items
  ini$method["rob2item"] <- "~ I(rob1 + rob2)"          # passive: 2-item comparable composite

  #' **Run imputation** [G&A]
  cat("\nRunning MICE (m = 20; this may take several minutes)...\n")
  dati_mice <- mice(d[, items_imp],
                    m               = 20,
                    print           = TRUE,
                    seed            = 42,
                    predictorMatrix = ini$predictorMatrix,
                    method          = ini$method,
                    visitSequence   = ini$visitSequence)
  rm(ini, d)

  #' **Convert to list of completed datasets** [G&A; extended]
  dati <- list()
  for (i in seq_len(dati_mice$m)) {
      dati[[i]] <- complete(dati_mice, i)

      # feel3 and feel4 are structurally absent in wave 1 [G&A]
      dati[[i]]$feel3[dati[[i]]$wave == 1] <- NA
      dati[[i]]$feel4[dati[[i]]$wave == 1] <- NA

      # feel items are not available as a comparable scale in wave 4 [EXTENSION]
      dati[[i]]$feel1[dati[[i]]$wave == 4] <- NA
      dati[[i]]$feel2[dati[[i]]$wave == 4] <- NA
      dati[[i]]$feel3[dati[[i]]$wave == 4] <- NA
      dati[[i]]$feel4[dati[[i]]$wave == 4] <- NA

      # Merge country-level data
      dati[[i]] <- merge(dati[[i]], cntry_long, by = c("cid", "wave"), all.x = TRUE)
  }
  dati <- as.mitml.list(dati)
  rm(i, cntry_long)


  #' ===================================================================
  #' # 7. Save data
  #' ===================================================================

  save(dat, dati, dati_mice, file = dat_rdata_path)
  cat("\nSaved:", dat_rdata_path, "\n")
  cat("  dat:        ", nrow(dat), "rows (raw data)\n")
  cat("  dati:       ", length(dati), "imputed datasets\n")
  cat("  dati_mice:  mice object\n")

}

rm(dat_rdata_path)




#' ===================================================================
#' # 8. Basic validation
#' ===================================================================

cat("\n=== VALIDATION ===\n")

#' **Mean composite score by wave** (unweighted, for quick sanity check)
#' Waves 1-3: rob (three-item, range 0-9).
#' Wave 4:    rob2item (two-item comparable composite, range 0-6).
#' The two composites are NOT directly comparable in magnitude;
#' they are printed separately to avoid misleading juxtaposition.
cat("\nUnweighted mean composite score by wave:\n")
cat("  [Waves 1-3: rob (0-9); Wave 4: rob2item (0-6) — different scales]\n")
for (w in 1:3) {
    m <- mean(dat$rob[dat$wave == w], na.rm = TRUE)
    n <- sum(!is.na(dat$rob[dat$wave == w]))
    cat(sprintf("  Wave %d: M(rob)     = %.2f  N = %d\n", w, m, n))
}
m4 <- mean(dat$rob2item[dat$wave == 4], na.rm = TRUE)
n4 <- sum(!is.na(dat$rob2item[dat$wave == 4]))
cat(sprintf("  Wave 4: M(rob2item) = %.2f  N = %d\n", m4, n4))

#' **Italy vs. EU comparison**
cat("\nItaly vs. EU (unweighted):\n")
cat("  [Waves 1-3: rob; Wave 4: rob2item — not directly comparable across rows]\n")
for (w in 1:3) {
    it <- mean(dat$rob[dat$wave == w & dat$cntry == "IT"], na.rm = TRUE)
    eu <- mean(dat$rob[dat$wave == w], na.rm = TRUE)
    cat(sprintf("  Wave %d (rob):     IT = %.2f  EU = %.2f  diff = %+.2f\n",
                w, it, eu, it - eu))
}
it4 <- mean(dat$rob2item[dat$wave == 4 & dat$cntry == "IT"], na.rm = TRUE)
eu4 <- mean(dat$rob2item[dat$wave == 4], na.rm = TRUE)
cat(sprintf("  Wave 4 (rob2item): IT = %.2f  EU = %.2f  diff = %+.2f\n",
            it4, eu4, it4 - eu4))

#' **Check rob2item consistency** [EXTENSION]
cat("\nConsistency check — rob2item == rob1 + rob2 (wave 4, unimputed):\n")
sub4 <- dat[dat$wave == 4 & !is.na(dat$rob2item) & !is.na(dat$rob1) & !is.na(dat$rob2), ]
ok <- all(sub4$rob2item == sub4$rob1 + sub4$rob2)
cat(sprintf("  Consistent: %s\n", ok))

#' **UAI values present**
cat("\nUAI snapshot (first 5 rows, wave 4):\n")
print(head(dat[dat$wave == 4,
               c("cntry", "UAI", "AGEOLD", "TECHEXP", "INVEST", "UNEMP", "LAT", "LONG")], 5))

cat("\nScript 1 complete. Proceed to 2_1_Descriptives_extended.R\n")
