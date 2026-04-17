#' ---
#' title: Descriptive results — Extended (4 waves, 2012-2024)
#' author: Camilla [estende Gnambs & Appel 2019]
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
source("./syntax/0_Start.R")   # funzioni helper: describe.imp, cor.imp, omg.imp

#' **Load data**
load("./data/dat.Rdata")
rm(dati_mice)   # [NOTA] G&A usano dati.mice, qui dati_mice (underscore)

# [FIX] Aggiunge rob2item a ogni dataset imputato
# Non incluso nell'imputazione (somma di rob1+rob2, non ha missing propri)
dati <- lapply(dati, function(x) {
  x$rob2item <- x$rob1 + x$rob2
  x
})
dati <- as.mitml.list(dati)


#' ===================================================================
#' # 1. Sample description
#' ===================================================================

#' **Paesi inclusi**
sort(unique(dat$cntry))
length(unique(dat$cntry))   # N paesi

#' **N per paese × wave**
#' [ESTENSIONE] Include wave 4; UK assente (post-Brexit)
table(dat$cntry, dat$wave)
describe(c(table(dat$cntry, dat$wave)))

#' **N totale**
nrow(dat)

#' **N per wave**
table(dat$wave)

#' **N Italia per wave**
#' [ESTENSIONE — Focus Italia]
cat("\n=== ITALIA ===\n")
table(dat$wave[dat$cntry == "IT"])




#' ===================================================================
#' # 2. Sociodemographic characteristics
#' ===================================================================

#' **Sesso** (0 = uomo, 1 = donna)
#' [NOTA] wave 4: d10 include opzione non-binary (N=49), ricodificato NA
prop.table(table(dat$sex))
prop.table(table(dat$sex, dat$wave), margin = 2)   # [ESTENSIONE] per wave

#' **Età**
describe(dat$age)
tapply(dat$age, dat$wave, mean, na.rm = TRUE)   # [ESTENSIONE] media per wave

#' **Istruzione** (anni di fine studi)
describe(dat$educ)
tapply(dat$educ, dat$wave, mean, na.rm = TRUE)

#' **Status occupazionale**
#' (1 = white-collar, 2 = blue-collar, 3 = non-employed)
prop.table(table(dat$white))
prop.table(table(dat$white, dat$wave), margin = 2)   # [ESTENSIONE] per wave




#' ===================================================================
#' # 3. Reliability of the attitude scale
#' ===================================================================
#' [G&A] Omega categoriale (poly = TRUE) su rob1–rob3 per wave 1–3.
#' [ESTENSIONE] Wave 4: rob3 ha formulazione diversa ("boring/repetitive"
#' invece di "hard/dangerous"). Si calcola omega su rob1+rob2 (2 item)
#' per confronto longitudinale pulito.

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

#' **Wave 4 — 2024** (rob1, rob2 — scala comparabile) [ESTENSIONE]
#' rob3 escluso: "boring/repetitive" non comparabile con "hard/dangerous"
#' delle wave 1-3. Reliability calcolata sul composite a 2 item (rob2item).
cat("\n--- Reliability Wave 4 (2024) — 2 item comparabili ---\n")
omg.imp(dati, items = paste0("rob", 1:2), poly = TRUE,
        weights = "wgt1",
        subset  = (dati[[1]]$wave == 4))

#' **Nota metodologica** [ESTENSIONE]
#' Per l'analisi longitudinale principale si usa `rob` (3 item, wave 1-3)
#' con verifica di invarianza di misura (script 2_2).
#' Per i confronti che includono wave 4 si usa `rob2item` (2 item),
#' documentando il compromesso in appendice metodologica.




#' ===================================================================
#' # 4. Descriptives and correlations — by wave
#' ===================================================================

#' **Dummy variables** [G&A]
dati <- within(dati, {
  sex1     <- ifelse(sex == 1, 1, 0)
  white2   <- ifelse(white == 2, 1, 0)
  white3   <- ifelse(white == 3, 1, 0)
  rob2item <- rob1 + rob2     # [FIX] ridefinito qui per sicurezza
})

#' -------------------------------------------------------------------
#' ## 4a. Medie e deviazioni standard
#' -------------------------------------------------------------------

#' **Wave 1 — 2012** [G&A]
#' NOTA: feel3, feel4 assenti in wave 1
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

#' **Wave 4 — 2024** [ESTENSIONE]
#' feel1-4 non comparabili in wave 4 (NA). Si usa rob2item.
cat("\n--- Descriptives Wave 4 (2024) ---\n")
describe.imp(dati,
             items   = c("rob", "rob2item",
                         "sex1", "age", "educ", "white2", "white3"),
             weights = "wgt2",
             stats   = c("mean", "sd"),
             subset  = (dati[[1]]$wave == 4))


#' -------------------------------------------------------------------
#' ## 4b. Correlazioni tra variabili di studio
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

#' **Wave 4 — 2024** [ESTENSIONE]
cat("\n--- Correlations Wave 4 (2024) ---\n")
cor.imp(dati,
        items   = c("rob", "rob2item",
                    "sex1", "age", "educ", "white2", "white3"),
        weights = "wgt2", digits = 3,
        subset  = (dati[[1]]$wave == 4))


#' -------------------------------------------------------------------
#' ## 4c. Missing data per wave
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

#' **Wave 4** [ESTENSIONE]
#' feel1-4 omesse (NA strutturali, non dati mancanti)
cat("\n--- Missing Wave 4 ---\n")
apply(dat[dat$wave == 4,
          c("rob", "rob2item", "sex", "age", "educ", "white")],
      2, function(x) round(mean(is.na(x)), 3))




#' ===================================================================
#' # 5. Longitudinal overview — composite score per wave e per paese
#' ===================================================================
#' [ESTENSIONE] Tabella riassuntiva del composite score (rob, ponderato)
#' per tutte e 4 le wave, EU27 e sotto-campione italiano.

#' **Medie ponderate per wave — EU27**
cat("\n=== COMPOSITE SCORE MEDIO PER WAVE (wgt2) ===\n")
for (w in 1:4) {
    sub <- dat[dat$wave == w & !is.na(dat$rob) & !is.na(dat$wgt2), ]
    m   <- weighted.mean(sub$rob, sub$wgt2)
    cat(sprintf("  Wave %d: M = %.3f  (N = %d)\n", w, m, nrow(sub)))
}

#' **Medie ponderate per wave — solo Italia** [ESTENSIONE]
cat("\n=== ITALIA: COMPOSITE SCORE MEDIO PER WAVE ===\n")
for (w in 1:4) {
    sub <- dat[dat$wave == w & dat$cntry == "IT" &
               !is.na(dat$rob) & !is.na(dat$wgt2), ]
    m   <- weighted.mean(sub$rob, sub$wgt2)
    eu  <- weighted.mean(
               dat[dat$wave == w & !is.na(dat$rob) & !is.na(dat$wgt2), "rob"],
               dat[dat$wave == w & !is.na(dat$rob) & !is.na(dat$wgt2), "wgt2"])
    cat(sprintf("  Wave %d: IT = %.3f  EU = %.3f  diff = %+.3f  (N_IT = %d)\n",
                w, m, eu, m - eu, nrow(sub)))
}

#' **Paese con score più alto e più basso per wave** [ESTENSIONE]
cat("\n=== RANKING PAESI PER WAVE ===\n")
for (w in 1:4) {
    sub <- dat[dat$wave == w & !is.na(dat$rob) & !is.na(dat$wgt2), ]
    means_by_country <- tapply(sub$rob * sub$wgt2, sub$cntry, sum) /
                        tapply(sub$wgt2,   sub$cntry, sum)
    cat(sprintf("\nWave %d — Top 3:\n", w))
    print(round(sort(means_by_country, decreasing = TRUE)[1:3], 3))
    cat(sprintf("Wave %d — Bottom 3:\n", w))
    print(round(sort(means_by_country)[1:3], 3))
}




#' ===================================================================
#' # 6. Country-level contextual variables
#' ===================================================================
#' [ESTENSIONE] Verifica che le variabili contestuali siano correttamente
#' presenti nel dataset e controllare i valori per l'Italia.

#' **Snapshot contestuale per l'Italia** [ESTENSIONE]
cat("\n=== VARIABILI CONTESTUALI — ITALIA ===\n")
it_ctx <- dat[dat$cntry == "IT" & !duplicated(paste(dat$cntry, dat$wave)),
              c("cntry", "wave", "AGEOLD", "UNEMP", "TECHEXP", "INVEST",
                "LAT", "LONG", "UAI")]
print(it_ctx[order(it_ctx$wave), ])

#' **UAI per tutti i paesi** (time-invariant) [ESTENSIONE]
cat("\n=== UAI PER PAESE ===\n")
uai_tab <- dat[!duplicated(dat$cntry), c("cntry", "UAI")]
print(uai_tab[order(uai_tab$UAI, decreasing = TRUE), ])

#' **Correlazione tra UAI e composite score medio (wave 3 — replicà G&A)**
cat("\n=== CORRELAZIONE UAI × COMPOSITE SCORE (Wave 3) ===\n")
sub <- dat[dat$wave == 3 & !is.na(dat$rob) & !is.na(dat$wgt2), ]
cntry_means <- tapply(sub$rob * sub$wgt2, sub$cntry, sum) /
               tapply(sub$wgt2,   sub$cntry, sum)
uai_vals    <- tapply(sub$UAI, sub$cntry, mean, na.rm = TRUE)
common      <- intersect(names(cntry_means), names(uai_vals))
cat(sprintf("r(UAI, composite_wave3) = %.3f\n",
            cor(cntry_means[common], uai_vals[common], use = "complete.obs")))

#' **Stessa correlazione per wave 4** [ESTENSIONE]
cat("\n=== CORRELAZIONE UAI × COMPOSITE SCORE (Wave 4) ===\n")
sub4 <- dat[dat$wave == 4 & !is.na(dat$rob) & !is.na(dat$wgt2), ]
cntry_means4 <- tapply(sub4$rob * sub4$wgt2, sub4$cntry, sum) /
                tapply(sub4$wgt2,   sub4$cntry, sum)
uai_vals4    <- tapply(sub4$UAI, sub4$cntry, mean, na.rm = TRUE)
common4      <- intersect(names(cntry_means4), names(uai_vals4))
cat(sprintf("r(UAI, composite_wave4) = %.3f\n",
            cor(cntry_means4[common4], uai_vals4[common4], use = "complete.obs")))




#' ===================================================================
#' # 7. Focus Italia — descrittive sociodemografiche
#' ===================================================================
#' [ESTENSIONE] Profilo del sotto-campione italiano per wave.
#' Da usare per la sezione Focus Italia nel Capitolo 5.

cat("\n=== PROFILO SOCIODEMOGRAFICO ITALIA PER WAVE ===\n")

it <- dat[dat$cntry == "IT", ]

#' **Sesso**
cat("\nSesso (proporzione donne):\n")
print(tapply(it$sex, it$wave, mean, na.rm = TRUE))

#' **Età media**
cat("\nEtà media:\n")
print(tapply(it$age, it$wave, mean, na.rm = TRUE))

#' **Istruzione media**
cat("\nIstruzione media (anni fine studi):\n")
print(tapply(it$educ, it$wave, mean, na.rm = TRUE))

#' **Status occupazionale**
cat("\nStatus occupazionale (proporzioni per wave):\n")
for (w in 1:4) {
    cat(sprintf("  Wave %d:\n", w))
    print(round(prop.table(table(it$white[it$wave == w])), 3))
}

#' **Composite score Italia per wave (ponderato)**
cat("\n=== COMPOSITE SCORE ITALIA (rob, ponderato) ===\n")

for (w in 1:4) {
  # media pooled sui 20 dataset imputati
  m_imp <- sapply(dati, function(x) {
    sub <- x[x$cntry == "IT" & x$wave == w, ]
    weighted.mean(sub$rob, sub$wgt2, na.rm = TRUE)
  })
  sd_imp <- sapply(dati, function(x) {
    sub <- x[x$cntry == "IT" & x$wave == w, ]
    sqrt(wtd.var(sub$rob, sub$wgt2))
  })
  n <- sum(dat$cntry == "IT" & dat$wave == w & !is.na(dat$rob))
  cat(sprintf("  Wave %d: M = %.3f  SD = %.3f  N = %d\n",
              w, mean(m_imp), mean(sd_imp), n))
}

# Stesso per rob2item in wave 4
cat("\n  Wave 4 (rob2item): ")
m2 <- sapply(dati, function(x) {
  sub <- x[x$cntry == "IT" & x$wave == 4, ]
  weighted.mean(sub$rob2item, sub$wgt2, na.rm = TRUE)
})
cat(sprintf("M = %.3f\n", mean(m2)))

cat("\n✓ Script 2_1 completato. Procedere con 2_2_Measurement_invariance_extended.R\n")
