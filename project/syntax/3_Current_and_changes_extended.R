#' ---
#' title: Current attitudes and changes over time — Extended (4 waves, 2012-2024)
#' author: Camilla Bonomo [extends Gnambs & Appel 2019]
#' output:
#'    html_document:
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---
#'
#' Country identifier (cid) mapping [EXTENSION NOTE]:
#' cid values differ from G&A because Croatia (HR) is included and
#' the United Kingdom (GB) is excluded. Relevant mappings:
#'   DK=7, FR=11, GR=12, IT=16, PT=23, SE=25, DE=6, ES=9
#'
#' Correction of a coding error in G&A:
#'   G&A use cid=13 for "Italy" (their script, line 215), but in their
#'   isocntry vector cid=13 corresponds to Hungary (HU), not Italy.
#'   The correct identifier for Italy in this dataset is cid=16.


#' **Clear workspace**
rm(list = ls())

#' **Load packages**
library(weights)
library(psych)
library(doBy)
library(mitml)
source("./syntax/0_Start.R")

#' **Load data**
load("./data/dat.Rdata")
rm(dati_mice)

#' **Add rob2item to imputed datasets**
#' rob2item is available as a passive-imputed variable in dati;
#' this step recomputes it from components as a consistency safeguard.
dati <- lapply(dati, function(x) {
    x$rob2item <- x$rob1 + x$rob2
    x
})
dati <- as.mitml.list(dati)

#' **Binary dummy variables for individual-level predictors**
dati <- within(dati, {
    sex1   <- ifelse(sex == 1, 1, 0)
    white2 <- ifelse(white == 2, 1, 0)
    white3 <- ifelse(white == 3, 1, 0)
})

#' **Country indicator reference** (for subsetting)
# DK=7, FR=11, GR=12, IT=16, PT=23, SE=25, DE=6, ES=9




#' ===================================================================
#' # 1. Mean attitudes by wave [G&A + EXTENSION]
#' ===================================================================

#' **Wave 3 — 2017** [G&A original section]
cat("\n=== MEAN ATTITUDES — WAVE 3 (2017) ===\n")
describe.imp(dati,
             items   = c("rob", paste0("feel", 1:4), "wave"),
             weights = "wgt2",
             stats   = c("mean", "sd"),
             subset  = (dati[[1]]$wave == 3))

#' **Wave 4 — 2024** [EXTENSION]
#' feel items are not available as a comparable scale in wave 4.
#' rob2item is used as the comparable composite for this wave.
cat("\n=== MEAN ATTITUDES — WAVE 4 (2024) ===\n")
describe.imp(dati,
             items   = c("rob", "rob2item", "wave"),
             weights = "wgt2",
             stats   = c("mean", "sd"),
             subset  = (dati[[1]]$wave == 4))




#' ===================================================================
#' # 2. Between-country variance (ICC) by wave [G&A + EXTENSION]
#' ===================================================================
#' The ICC quantifies the proportion of variance in the composite score
#' attributable to country membership. A declining ICC over time indicates
#' convergence of national attitudes (homogenisation across EU member states).
#'
#' Waves 1-3: ICC computed on rob (three-item composite, comparable).
#' Wave 4: ICC computed on rob2item (two-item comparable composite),
#'   because rob includes a non-comparable rob3 item in wave 4.

#' **Wave 1 — 2012** [G&A]
cat("\n=== ICC — WAVE 1 (2012) ===\n")
lmer.imp(rob ~ 1 + (1 | cid), data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 1))

#' **Wave 2 — 2014** [EXTENSION]
cat("\n=== ICC — WAVE 2 (2014) ===\n")
lmer.imp(rob ~ 1 + (1 | cid), data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 2))

#' **Wave 3 — 2017** [G&A]
cat("\n=== ICC — WAVE 3 (2017) ===\n")
lmer.imp(rob ~ 1 + (1 | cid), data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 3))

#' **Wave 4 — 2024** [EXTENSION]
#' rob2item is used because rob3 changed wording in wave 4.
cat("\n=== ICC — WAVE 4 (2024) ===\n")
lmer.imp(rob2item ~ 1 + (1 | cid), data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 4))

#' **ICC for feel items — wave 3** [G&A]
cat("\n=== ICC FEEL ITEMS — WAVE 3 (2017) ===\n")
cat("feel1 (medical operation):\n")
lmer.imp(feel1 ~ 1 + (1 | cid), data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 3))
cat("\nfeel2 (at work):\n")
lmer.imp(feel2 ~ 1 + (1 | cid), data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 3))
cat("\nfeel3 (assisting elderly):\n")
lmer.imp(feel3 ~ 1 + (1 | cid), data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 3))
cat("\nfeel4 (driverless cars):\n")
lmer.imp(feel4 ~ 1 + (1 | cid), data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 3))




#' ===================================================================
#' # 3. Between-country differences in the most recent wave
#' ===================================================================
#' [G&A] Analyses of wave 3 replicated; extended to wave 4 [EXTENSION].
#' Italy added as a focal country [EXTENSION].

#' **Country indicators** [G&A adapted + EXTENSION]
dati <- within(dati, {
    isDenmark  <- ifelse(cid ==  7, 1, 0)   # DK
    isSweden   <- ifelse(cid == 25, 1, 0)   # SE  [G&A: cid=26 with UK]
    isGreece   <- ifelse(cid == 12, 1, 0)   # GR  [G&A: cid=13 with UK]
    isFrance   <- ifelse(cid == 11, 1, 0)   # FR
    isItaly    <- ifelse(cid == 16, 1, 0)   # IT  [EXTENSION — focal country]
    isGermany  <- ifelse(cid ==  6, 1, 0)   # DE  [EXTENSION]
    isPortugal <- ifelse(cid == 23, 1, 0)   # PT  [EXTENSION]
    isSpain    <- ifelse(cid ==  9, 1, 0)   # ES  [EXTENSION]
})

#' -------------------------------------------------------------------
#' ## 3a. Between-country differences — wave 3 [G&A replication]
#' -------------------------------------------------------------------
cat("\n=== BETWEEN-COUNTRY DIFFERENCES — WAVE 3 (2017) ===\n")

cat("\nDenmark vs. rest of EU:\n")
ttest.imp(rob ~ isDenmark, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 3))

cat("\nSweden vs. rest of EU:\n")
ttest.imp(rob ~ isSweden, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 3))

cat("\nGreece vs. rest of EU:\n")
ttest.imp(rob ~ isGreece, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 3))

cat("\nFrance vs. rest of EU:\n")
ttest.imp(rob ~ isFrance, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 3))

#' **Italy vs. rest of EU — wave 3** [EXTENSION]
cat("\nItaly vs. rest of EU (wave 3):\n")
ttest.imp(rob ~ isItaly, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 3))


#' -------------------------------------------------------------------
#' ## 3b. Between-country differences — wave 4 [EXTENSION]
#' -------------------------------------------------------------------
#' rob2item used throughout (two-item comparable composite, range 0-6),
#' because rob3 wording changed in wave 4 and renders rob non-comparable.
cat("\n=== BETWEEN-COUNTRY DIFFERENCES — WAVE 4 (2024, rob2item) ===\n")

cat("\nDenmark vs. rest of EU:\n")
ttest.imp(rob2item ~ isDenmark, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 4))

cat("\nSweden vs. rest of EU:\n")
ttest.imp(rob2item ~ isSweden, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 4))

cat("\nGreece vs. rest of EU:\n")
ttest.imp(rob2item ~ isGreece, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 4))

cat("\nFrance vs. rest of EU:\n")
ttest.imp(rob2item ~ isFrance, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 4))

cat("\nItaly vs. rest of EU (wave 4):\n")
ttest.imp(rob2item ~ isItaly, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 4))

cat("\nGermany vs. rest of EU (wave 4):\n")
ttest.imp(rob2item ~ isGermany, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 4))

cat("\nPortugal vs. rest of EU (wave 4):\n")
ttest.imp(rob2item ~ isPortugal, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 4))




#' ===================================================================
#' # 4. Associations with individual-level characteristics [G&A + EXTENSION]
#' ===================================================================
#' G&A analysed wave 3 only. Extended here to wave 4 and the Italian subsample.

#' -------------------------------------------------------------------
#' ## 4a. Wave 3 [G&A replication]
#' -------------------------------------------------------------------
cat("\n=== ASSOCIATIONS WITH INDIVIDUAL CHARACTERISTICS — WAVE 3 ===\n")

cat("\nSex (wave 3):\n")
ttest.imp(rob ~ sex, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 3))

dati <- within(dati, {
    white_wc <- ifelse(white == 1, 1, 0)   # white-collar vs. other
})

cat("\nWhite-collar vs. other (wave 3):\n")
ttest.imp(rob ~ white_wc, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 3))

cat("\nAge and education (correlations, wave 3):\n")
cor.imp(dati, items = c("rob", "age", "educ"),
        weights = "wgt2",
        subset  = (dati[[1]]$wave == 3))


#' -------------------------------------------------------------------
#' ## 4b. Wave 4 [EXTENSION]
#' -------------------------------------------------------------------
#' rob2item used throughout (two-item comparable composite, range 0-6).
cat("\n=== ASSOCIATIONS WITH INDIVIDUAL CHARACTERISTICS — WAVE 4 (rob2item) ===\n")

cat("\nSex (wave 4):\n")
ttest.imp(rob2item ~ sex, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 4))

cat("\nWhite-collar vs. other (wave 4):\n")
ttest.imp(rob2item ~ white_wc, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 4))

cat("\nAge and education (correlations, wave 4):\n")
cor.imp(dati, items = c("rob2item", "age", "educ"),
        weights = "wgt2",
        subset  = (dati[[1]]$wave == 4))


#' -------------------------------------------------------------------
#' ## 4c. Italy focus — waves 3 and 4 [EXTENSION]
#' -------------------------------------------------------------------
cat("\n=== ASSOCIATIONS — ITALY, WAVE 3 ===\n")

cat("\nSex — Italy, wave 3:\n")
ttest.imp(rob ~ sex, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 3 & dati[[1]]$cid == 16))

cat("\nWhite-collar — Italy, wave 3:\n")
ttest.imp(rob ~ white_wc, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 3 & dati[[1]]$cid == 16))

cat("\nAge and education — Italy, wave 3:\n")
cor.imp(dati, items = c("rob", "age", "educ"),
        weights = "wgt2",
        subset  = (dati[[1]]$wave == 3 & dati[[1]]$cid == 16))

cat("\n=== ASSOCIATIONS — ITALY, WAVE 4 (rob2item) ===\n")

cat("\nSex — Italy, wave 4:\n")
ttest.imp(rob2item ~ sex, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 4 & dati[[1]]$cid == 16))

cat("\nWhite-collar — Italy, wave 4:\n")
ttest.imp(rob2item ~ white_wc, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 4 & dati[[1]]$cid == 16))

cat("\nAge and education — Italy, wave 4:\n")
cor.imp(dati, items = c("rob2item", "age", "educ"),
        weights = "wgt2",
        subset  = (dati[[1]]$wave == 4 & dati[[1]]$cid == 16))




#' ===================================================================
#' # 5. Differences across task-specific feel items — wave 3 [G&A]
#' ===================================================================

cat("\n=== TASK-SPECIFIC DIFFERENCES — WAVE 3 ===\n")

cat("\nAt work vs. medical operation:\n")
ttest.imp(feel2 ~ feel1, dati, weights = "wgt2", paired = TRUE,
          subset = (dati[[1]]$wave == 3))

cat("\nAt work vs. assisting elderly:\n")
ttest.imp(feel2 ~ feel3, dati, weights = "wgt2", paired = TRUE,
          subset = (dati[[1]]$wave == 3))

cat("\nAt work vs. driverless cars:\n")
ttest.imp(feel2 ~ feel4, dati, weights = "wgt2", paired = TRUE,
          subset = (dati[[1]]$wave == 3))




#' ===================================================================
#' # 6. Changes over time [G&A + EXTENSION]
#' ===================================================================

cat("\n=== CHANGES OVER TIME ===\n")

#' **Wave 1 vs. wave 3** [G&A]
cat("\nWave 1 vs. wave 3 (EU27):\n")
ttest.imp(rob ~ wave, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3)))

#' **Wave 1 vs. wave 2** [G&A]
cat("\nWave 1 vs. wave 2 (EU27):\n")
ttest.imp(rob ~ wave, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 2)))

#' **Wave 3 vs. wave 4** [EXTENSION]
#' rob2item used for comparability with wave 4.
cat("\nWave 3 vs. wave 4 (EU27, rob2item):\n")
ttest.imp(rob2item ~ wave, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(3, 4)))

#' **Wave 1 vs. wave 4** [EXTENSION]
cat("\nWave 1 vs. wave 4 (EU27, rob2item):\n")
ttest.imp(rob2item ~ wave, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 4)))

#' **Feel items: wave 1 vs. wave 3** [G&A]
cat("\nfeel2 (at work) wave 1 vs. wave 3:\n")
ttest.imp(feel2 ~ wave, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3)))

cat("\nfeel1 (medical) wave 1 vs. wave 3:\n")
ttest.imp(feel1 ~ wave, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3)))




#' ===================================================================
#' # 7. Changes by country [G&A + EXTENSION]
#' ===================================================================

cat("\n=== CHANGES BY COUNTRY (wave 1 vs. wave 3) ===\n")

#' [G&A] Countries from original analysis
cat("\nDenmark:\n")
ttest.imp(rob ~ wave, data = dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3) & dati[[1]]$cid == 7))

cat("\nSweden:\n")
ttest.imp(rob ~ wave, data = dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3) & dati[[1]]$cid == 25))

cat("\nGreece:\n")
ttest.imp(rob ~ wave, data = dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3) & dati[[1]]$cid == 12))

cat("\nPortugal:\n")
ttest.imp(rob ~ wave, data = dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3) & dati[[1]]$cid == 23))

#' [EXTENSION] Benchmark countries for the Italy focus
cat("\nItaly (wave 1 vs. wave 3):\n")
ttest.imp(rob ~ wave, data = dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3) & dati[[1]]$cid == 16))

cat("\nGermany (wave 1 vs. wave 3):\n")
ttest.imp(rob ~ wave, data = dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3) & dati[[1]]$cid == 6))

cat("\nFrance (wave 1 vs. wave 3):\n")
ttest.imp(rob ~ wave, data = dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3) & dati[[1]]$cid == 11))


#' -------------------------------------------------------------------
#' [EXTENSION] Changes wave 1 vs. wave 4 by benchmark country
#' rob2item used for cross-wave comparability with wave 4.
#' -------------------------------------------------------------------
cat("\n=== CHANGES BY COUNTRY (wave 1 vs. wave 4, rob2item) ===\n")

for (paese in list(c("Italy",    16),
                   c("Germany",   6),
                   c("France",   11),
                   c("Denmark",   7),
                   c("Sweden",   25),
                   c("Spain",     9),
                   c("Portugal", 23),
                   c("Greece",   12))) {
    nome    <- paese[1]
    cid_val <- as.integer(paese[2])
    cat(sprintf("\n%s (wave 1 vs. wave 4, rob2item):\n", nome))
    ttest.imp(rob2item ~ wave, data = dati,
              weights  = "wgt2",
              paired   = FALSE,
              subset   = (dati[[1]]$wave %in% c(1, 4) &
                          dati[[1]]$cid == cid_val))
}


cat("\nScript 3 complete. Proceed to 4_Predictors_extended.R\n")
