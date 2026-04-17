#' ---
#' title: Current and changes of attitudes — Extended (4 waves, 2012-2024)
#' author: Camilla [estende Gnambs & Appel 2019]
#' output:
#'    html_document:
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---
#'
#' **NOTA SUI CID**
#' I cid differiscono da G&A perché Camilla include HR ed esclude UK.
#' Mappatura rilevante (da isocntry in 1_Load_data_extended.R):
#'   DK=7, FR=11, GR=12, IT=16, PT=23, SE=25, DE=6, ES=9
#'
#' ERRORE NEL CODICE PUBBLICATO G&A:
#'   G&A usano cid=13 per "Italy" (riga 215) ma nel loro isocntry
#'   cid=13 corrisponde a HU (Hungary), non IT. Il confronto "Italy"
#'   nel loro script è probabilmente sbagliato. Qui usiamo cid=16.


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

#' **Aggiungi rob2item ai dataset imputati**
dati <- lapply(dati, function(x) {
    x$rob2item <- x$rob1 + x$rob2
    x
})
dati <- as.mitml.list(dati)

#' **Dummy variables**
dati <- within(dati, {
    sex1   <- ifelse(sex == 1, 1, 0)
    white2 <- ifelse(white == 2, 1, 0)
    white3 <- ifelse(white == 3, 1, 0)
})

#' **CID reference** (per selezione paesi)
# DK=7, FR=11, GR=12, IT=16, PT=23, SE=25, DE=6, ES=9




#' ===================================================================
#' # 1. Mean attitudes — per wave [G&A + ESTENSIONE]
#' ===================================================================

#' **Wave 3 — 2017** [G&A: sezione originale]
cat("\n=== MEDIE ATTITUDINALI — WAVE 3 (2017) ===\n")
describe.imp(dati,
             items   = c("rob", paste0("feel", 1:4), "wave"),
             weights = "wgt2",
             stats   = c("mean", "sd"),
             subset  = (dati[[1]]$wave == 3))

#' **Wave 4 — 2024** [ESTENSIONE]
#' feel1-4 non disponibili con scala comparabile in wave 4
cat("\n=== MEDIE ATTITUDINALI — WAVE 4 (2024) ===\n")
describe.imp(dati,
             items   = c("rob", "rob2item", "wave"),
             weights = "wgt2",
             stats   = c("mean", "sd"),
             subset  = (dati[[1]]$wave == 4))




#' ===================================================================
#' # 2. Variabilità tra paesi (ICC) — per wave [G&A + ESTENSIONE]
#' ===================================================================
#' ICC = quota di varianza nel composite score spiegata dal paese.
#' Un ICC in calo indica convergenza tra paesi nel tempo.

#' **Wave 1 — 2012** [G&A]
cat("\n=== ICC — WAVE 1 (2012) ===\n")
lmer.imp(rob ~ 1 | cid, data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 1))

#' **Wave 2 — 2014** [ESTENSIONE]
cat("\n=== ICC — WAVE 2 (2014) ===\n")
lmer.imp(rob ~ 1 | cid, data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 2))

#' **Wave 3 — 2017** [G&A]
cat("\n=== ICC — WAVE 3 (2017) ===\n")
lmer.imp(rob ~ 1 | cid, data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 3))

#' **Wave 4 — 2024** [ESTENSIONE]
cat("\n=== ICC — WAVE 4 (2024) ===\n")
lmer.imp(rob ~ 1 | cid, data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 4))

#' **ICC per feel items — wave 3** [G&A]
cat("\n=== ICC FEEL ITEMS — WAVE 3 (2017) ===\n")
cat("feel1 (medical operation):\n")
lmer.imp(feel1 ~ 1 | cid, data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 3))
cat("\nfeel2 (at work):\n")
lmer.imp(feel2 ~ 1 | cid, data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 3))
cat("\nfeel3 (assisting elderly):\n")
lmer.imp(feel3 ~ 1 | cid, data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 3))
cat("\nfeel4 (driverless cars):\n")
lmer.imp(feel4 ~ 1 | cid, data = dati, weights = "wgt2",
         subset = (dati[[1]]$wave == 3))




#' ===================================================================
#' # 3. Differenze tra paesi nella wave più recente [G&A + ESTENSIONE]
#' ===================================================================
#' G&A analizzano la wave 3 (2017). Si replica per wave 3 e si estende
#' alla wave 4 (2024), aggiungendo l'Italia come focus principale.

#' **Indicatori paese** [G&A adattato + ESTENSIONE]
dati <- within(dati, {
    isDenmark  <- ifelse(cid ==  7, 1, 0)   # DK
    isSweden   <- ifelse(cid == 25, 1, 0)   # SE  [G&A: cid=26 con UK]
    isGreece   <- ifelse(cid == 12, 1, 0)   # GR  [G&A: cid=13 con UK]
    isFrance   <- ifelse(cid == 11, 1, 0)   # FR
    isItaly    <- ifelse(cid == 16, 1, 0)   # IT  [ESTENSIONE — focus tesi]
    isGermany  <- ifelse(cid ==  6, 1, 0)   # DE  [ESTENSIONE]
    isPortugal <- ifelse(cid == 23, 1, 0)   # PT  [ESTENSIONE]
    isSpain    <- ifelse(cid ==  9, 1, 0)   # ES  [ESTENSIONE]
})

#' -------------------------------------------------------------------
#' ## 3a. Differenze per wave 3 (replica G&A)
#' -------------------------------------------------------------------
cat("\n=== DIFFERENZE TRA PAESI — WAVE 3 (2017) ===\n")

cat("\nDanimarca vs resto EU:\n")
ttest.imp(rob ~ isDenmark, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 3))

cat("\nSvezia vs resto EU:\n")
ttest.imp(rob ~ isSweden, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 3))

cat("\nGrecia vs resto EU:\n")
ttest.imp(rob ~ isGreece, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 3))

cat("\nFrancia vs resto EU:\n")
ttest.imp(rob ~ isFrance, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 3))

#' **Italia vs resto EU — wave 3** [ESTENSIONE]
cat("\nItalia vs resto EU (wave 3):\n")
ttest.imp(rob ~ isItaly, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 3))


#' -------------------------------------------------------------------
#' ## 3b. Differenze per wave 4 — ESTENSIONE
#' -------------------------------------------------------------------
cat("\n=== DIFFERENZE TRA PAESI — WAVE 4 (2024) ===\n")

cat("\nDanimarca vs resto EU:\n")
ttest.imp(rob ~ isDenmark, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 4))

cat("\nSvezia vs resto EU:\n")
ttest.imp(rob ~ isSweden, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 4))

cat("\nGrecia vs resto EU:\n")
ttest.imp(rob ~ isGreece, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 4))

cat("\nFrancia vs resto EU:\n")
ttest.imp(rob ~ isFrance, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 4))

cat("\nItalia vs resto EU (wave 4):\n")
ttest.imp(rob ~ isItaly, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 4))

cat("\nGermania vs resto EU (wave 4):\n")
ttest.imp(rob ~ isGermany, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 4))

cat("\nPortogallo vs resto EU (wave 4):\n")
ttest.imp(rob ~ isPortugal, data = dati, weights = "wgt2",
          subset = (dati[[1]]$wave == 4))




#' ===================================================================
#' # 4. Associazioni con caratteristiche individuali [G&A + ESTENSIONE]
#' ===================================================================
#' G&A analizzano solo wave 3. Si estende a wave 4 e al sotto-campione IT.

#' -------------------------------------------------------------------
#' ## 4a. Wave 3 (replica G&A)
#' -------------------------------------------------------------------
cat("\n=== ASSOCIAZIONI CON CARATTERISTICHE INDIVIDUALI — WAVE 3 ===\n")

cat("\nSesso (wave 3):\n")
ttest.imp(rob ~ sex, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 3))

dati <- within(dati, {
    white_wc <- ifelse(white == 1, 1, 0)   # white-collar vs altri
})

cat("\nWhite-collar vs altri (wave 3):\n")
ttest.imp(rob ~ white_wc, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 3))

cat("\nEtà e istruzione (correlazioni, wave 3):\n")
cor.imp(dati, items = c("rob", "age", "educ"),
        weights = "wgt2",
        subset  = (dati[[1]]$wave == 3))


#' -------------------------------------------------------------------
#' ## 4b. Wave 4 (ESTENSIONE)
#' -------------------------------------------------------------------
cat("\n=== ASSOCIAZIONI CON CARATTERISTICHE INDIVIDUALI — WAVE 4 ===\n")

cat("\nSesso (wave 4):\n")
ttest.imp(rob ~ sex, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 4))

cat("\nWhite-collar vs altri (wave 4):\n")
ttest.imp(rob ~ white_wc, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 4))

cat("\nEtà e istruzione (correlazioni, wave 4):\n")
cor.imp(dati, items = c("rob", "age", "educ"),
        weights = "wgt2",
        subset  = (dati[[1]]$wave == 4))


#' -------------------------------------------------------------------
#' ## 4c. Focus Italia — wave 3 e wave 4 (ESTENSIONE)
#' -------------------------------------------------------------------
cat("\n=== ASSOCIAZIONI — ITALIA, WAVE 3 ===\n")

cat("\nSesso — IT wave 3:\n")
ttest.imp(rob ~ sex, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 3 & dati[[1]]$cid == 16))

cat("\nWhite-collar — IT wave 3:\n")
ttest.imp(rob ~ white_wc, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 3 & dati[[1]]$cid == 16))

cat("\nEtà e istruzione — IT wave 3:\n")
cor.imp(dati, items = c("rob", "age", "educ"),
        weights = "wgt2",
        subset  = (dati[[1]]$wave == 3 & dati[[1]]$cid == 16))

cat("\n=== ASSOCIAZIONI — ITALIA, WAVE 4 ===\n")

cat("\nSesso — IT wave 4:\n")
ttest.imp(rob ~ sex, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 4 & dati[[1]]$cid == 16))

cat("\nWhite-collar — IT wave 4:\n")
ttest.imp(rob ~ white_wc, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave == 4 & dati[[1]]$cid == 16))

cat("\nEtà e istruzione — IT wave 4:\n")
cor.imp(dati, items = c("rob", "age", "educ"),
        weights = "wgt2",
        subset  = (dati[[1]]$wave == 4 & dati[[1]]$cid == 16))




#' ===================================================================
#' # 5. Differenze tra attitude task-specifiche (feel) — wave 3 [G&A]
#' ===================================================================

cat("\n=== DIFFERENZE TASK-SPECIFICHE — WAVE 3 ===\n")

cat("\nLavoro vs operazione medica:\n")
ttest.imp(feel2 ~ feel1, dati, weights = "wgt2", paired = TRUE,
          subset = (dati[[1]]$wave == 3))

cat("\nLavoro vs assistenza anziani:\n")
ttest.imp(feel2 ~ feel3, dati, weights = "wgt2", paired = TRUE,
          subset = (dati[[1]]$wave == 3))

cat("\nLavoro vs guida autonoma:\n")
ttest.imp(feel2 ~ feel4, dati, weights = "wgt2", paired = TRUE,
          subset = (dati[[1]]$wave == 3))




#' ===================================================================
#' # 6. Cambiamenti nel tempo [G&A + ESTENSIONE]
#' ===================================================================

cat("\n=== CAMBIAMENTI NEL TEMPO ===\n")

#' **Wave 1 vs Wave 3** [G&A]
cat("\nWave 1 vs Wave 3 (EU):\n")
ttest.imp(rob ~ wave, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3)))

#' **Wave 1 vs Wave 2** [G&A]
cat("\nWave 1 vs Wave 2 (EU):\n")
ttest.imp(rob ~ wave, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 2)))

#' **Wave 3 vs Wave 4** [ESTENSIONE]
#' Usa rob2item per comparabilità con wave 4
cat("\nWave 3 vs Wave 4 (EU, rob2item):\n")
ttest.imp(rob2item ~ wave, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(3, 4)))

#' **Wave 1 vs Wave 4** [ESTENSIONE]
cat("\nWave 1 vs Wave 4 (EU, rob2item):\n")
ttest.imp(rob2item ~ wave, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 4)))

#' **Feel items: wave 1 vs wave 3** [G&A]
cat("\nfeel2 (lavoro) wave 1 vs wave 3:\n")
ttest.imp(feel2 ~ wave, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3)))

cat("\nfeel1 (medico) wave 1 vs wave 3:\n")
ttest.imp(feel1 ~ wave, dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3)))




#' ===================================================================
#' # 7. Cambiamenti per paese [G&A + ESTENSIONE]
#' ===================================================================

cat("\n=== CAMBIAMENTI PER PAESE (wave 1 vs wave 3) ===\n")

#' [G&A] Paesi originali
cat("\nDanimarca:\n")
ttest.imp(rob ~ wave, data = dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3) & dati[[1]]$cid == 7))

cat("\nSvezia:\n")
ttest.imp(rob ~ wave, data = dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3) & dati[[1]]$cid == 25))

cat("\nGrecia:\n")
ttest.imp(rob ~ wave, data = dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3) & dati[[1]]$cid == 12))

cat("\nPortogallo:\n")
ttest.imp(rob ~ wave, data = dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3) & dati[[1]]$cid == 23))

#' [ESTENSIONE] Paesi benchmark per Focus Italia
cat("\nItalia (wave 1 vs wave 3):\n")
ttest.imp(rob ~ wave, data = dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3) & dati[[1]]$cid == 16))

cat("\nGermania (wave 1 vs wave 3):\n")
ttest.imp(rob ~ wave, data = dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3) & dati[[1]]$cid == 6))

cat("\nFrancia (wave 1 vs wave 3):\n")
ttest.imp(rob ~ wave, data = dati, weights = "wgt2", paired = FALSE,
          subset = (dati[[1]]$wave %in% c(1, 3) & dati[[1]]$cid == 11))


#' -------------------------------------------------------------------
#' [ESTENSIONE] Cambiamenti wave 1 vs wave 4 per paese benchmark
#' -------------------------------------------------------------------
cat("\n=== CAMBIAMENTI PER PAESE (wave 1 vs wave 4, rob2item) ===\n")

for (paese in list(c("Italia",    16),
                   c("Germania",   6),
                   c("Francia",   11),
                   c("Danimarca",  7),
                   c("Svezia",    25),
                   c("Spagna",     9),
                   c("Portogallo",23),
                   c("Grecia",    12))) {
    nome <- paese[1]
    cid_val <- as.integer(paese[2])
    cat(sprintf("\n%s (wave 1 vs wave 4, rob2item):\n", nome))
    ttest.imp(rob2item ~ wave, data = dati,
              weights  = "wgt2",
              paired   = FALSE,
              subset   = (dati[[1]]$wave %in% c(1, 4) &
                          dati[[1]]$cid == cid_val))
}


cat("\n✓ Script 3 completato. Procedere con 4__Predictors_extended.R\n")
