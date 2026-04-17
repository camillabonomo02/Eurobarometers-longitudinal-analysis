#' ---
#' title: "Prepare data for analyses — Replica & estensione Gnambs & Appel (2019)"
#' author: "[Camilla] — CIM4.0"
#' output: 
#'    html_document: 
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---
#'
#' Questo script adatta il codice originale di Gnambs & Appel (2019) per:
#'   1. Aggiungere la wave 2024 (EB 101.4 / SP EB 554)
#'   2. Aggiungere Hofstede UAI come predittore contestuale
#'   3. Escludere UK (assente nel 2024 post-Brexit) per coerenza
#'   4. Preparare il focus Italia
#'
#' I commenti marcati [G&A] indicano codice replicato dall'originale.
#' I commenti marcati [ESTENSIONE] indicano il contributo originale della tesi.
#'


#' **Clear workspace**
rm(list = ls())

#' **Load packages**
library(haven)       # [G&A] per read_spss
library(dplyr)       # [G&A]
library(doBy)        # [G&A] per recodeVar
library(mice)        # [G&A] per imputation
library(mitml)       # [G&A] per pooling risultati
library(readxl)      # [ESTENSIONE] per Eurostat xlsx

# [G&A] don't convert strings into factors
options(stringsAsFactors = FALSE)


#' ===================================================================
#' # CONFIGURAZIONE PATHS
#' ===================================================================
#' **Aggiornare questi paths in base alla propria struttura di directory**

# Directory con i file .sav/.dta scaricati da GESIS
RAWDATA_DIR <- "./rawdata"

# Directory con i dati contestuali (World Bank, Hofstede, Eurostat)
CONTEXTDATA_DIR <- "./contextdata"

# Directory output
DATA_DIR  <- "data"
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

#' **Wave 4: April-May 2024** [ESTENSIONE]
dat4 <- zap_labels(read_dta(file.path(RAWDATA_DIR, "ZA8844_v1-0-0.dta")))




#' ===================================================================
#' # 2. Load and prepare country-level data
#' ===================================================================

#' **Country codes** [G&A + ESTENSIONE: rimosso GB, aggiornato]
#' [ESTENSIONE] UK escluso: assente nella wave 2024 post-Brexit.
#' Per coerenza, escluso da tutte le wave.
isocntry <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", 
              "FI", "FR", "GR", "HR", "HU", "IE", "IT", "LT",
              "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", 
              "SK")
cid_seq <- seq_len(length(isocntry))

#' -------------------------------------------------------------------
#' ## 2a. World Bank data
#' -------------------------------------------------------------------
#' [G&A] Le variabili originali sono: AGEOLD, TECHEXP, RESEAR, RESEXP, UNEMP
#' Struttura: il file Excel/CSV originale di G&A aveva queste variabili
#' per paese e per wave. Qui lo ricostruiamo dal file World Bank.
#'
#' **NOTA:** Il file World Bank deve contenere 5 indicatori:
#'   - SP.POP.65UP.TO.ZS  → AGEOLD  (% popolazione 65+)
#'   - SL.UEM.TOTL.ZS     → UNEMP   (tasso disoccupazione)
#'   - TX.VAL.TECH.MF.ZS  → TECHEXP (% export hi-tech)
#'   - GB.XPD.RSDV.GD.ZS  → RESEXP  (R&D expenditure % GDP)
#'   - SP.POP.SCIE.RD.P6     → RESEAR  (ricercatori R&D per milione)
#'
#' Con RESEAR + RESEXP si costruisce INVEST = media z-score (replica G&A).

wb <- read.csv(file.path(CONTEXTDATA_DIR, "worldbank.csv"),
               stringsAsFactors = FALSE)

# Pulizia
wb <- wb[!is.na(wb$Series.Code) & nchar(wb$Series.Code) > 3, ]

# ISO3 → ISO2
iso3to2 <- c(AUT="AT",BEL="BE",BGR="BG",HRV="HR",CYP="CY",
             CZE="CZ",DNK="DK",EST="EE",FIN="FI",FRA="FR",
             DEU="DE",GRC="GR",HUN="HU",IRL="IE",ITA="IT",
             LVA="LV",LTU="LT",LUX="LU",MLT="MT",NLD="NL",
             POL="PL",PRT="PT",ROU="RO",SVK="SK",SVN="SI",
             ESP="ES",SWE="SE")
wb$cntry <- iso3to2[wb$Country.Code]

# Indicator mapping
ind_map <- c(SP.POP.65UP.TO.ZS = "AGEOLD",
             SL.UEM.TOTL.ZS    = "UNEMP",
             TX.VAL.TECH.MF.ZS = "TECHEXP",
             GB.XPD.RSDV.GD.ZS = "RESEXP",
             SP.POP.SCIE.RD.P6    = "RESEAR")
wb$varname <- ind_map[wb$Series.Code]
wb <- wb[!is.na(wb$varname) & !is.na(wb$cntry), ]

# Year columns → numeric
year_cols <- c("X2012..YR2012.", "X2014..YR2014.", 
               "X2017..YR2017.","X2022..YR2022.","X2024..YR2024.")
year_labels <- c(12, 14, 17, 22, 24)

# Build country-level dataset in wide format à la G&A
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

#' **R&D 2024 missing** — World Bank non ha ancora pubblicato R&D per 2024.
#' [AZIONE] Sostituire con anno 2022.

if ("RESEXP24" %in% names(cntry_data) && all(is.na(cntry_data$RESEXP24))) {
    message("⚠ RESEXP 2024 mancante — uso valore 2022 come placeholder")
    cntry_data$RESEXP24 <- cntry_data$RESEXP22
}
if ("RESEAR24" %in% names(cntry_data) && all(is.na(cntry_data$RESEAR24))) {
    message("⚠ RESEAR 2024 mancante — uso valore 2022 come placeholder")
    cntry_data$RESEAR24 <- cntry_data$RESEAR22
}

#' **Costruisci INVEST** [G&A — replica esatta]
#' INVEST = media degli z-score di RESEAR (ricercatori) e RESEXP (spesa R&D)
#' NOTA: Serve il dato World Bank SP.RES.TOTL.FT (Researchers in R&D per million)
#'       nel file CSV con Series Code = "SP.RES.TOTL.FT" e varname = "RESEAR"
#'
#' Se RESEAR è disponibile nel dataset, costruisce INVEST come G&A.
#' Altrimenti fallback a INVEST = z-score di RESEXP (documentare).

has_resear <- any(grepl("RESEAR", names(cntry_data)))

if (has_resear) {
    message("✓ RESEAR trovato — costruisco INVEST come G&A (media z-score RESEAR + RESEXP)")
    cntry_data$INVEST12 <- rowMeans(apply(cntry_data[, c("RESEAR12", "RESEXP12")], 2, scale))
    cntry_data$INVEST14 <- rowMeans(apply(cntry_data[, c("RESEAR14", "RESEXP14")], 2, scale))
    cntry_data$INVEST17 <- rowMeans(apply(cntry_data[, c("RESEAR17", "RESEXP17")], 2, scale))
    cntry_data$INVEST24 <- rowMeans(apply(cntry_data[, c("RESEAR24", "RESEXP24")], 2, scale))
} else {
    message("⚠ RESEAR non trovato — INVEST = z-score di RESEXP (proxy)")
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
    LAT = c(47.5,50.5,42.7,35.1,49.8,51.2,56.3,58.6,40.5,
            61.9,46.2,39.1,45.1,47.2,53.1,41.9,55.2,
            49.8,56.9,35.9,52.1,51.9,39.4,45.9,60.1,46.2,48.7),
    LONG = c(14.6,4.5,25.5,33.4,15.5,10.5,9.5,25.0,-3.7,
             25.7,2.2,21.8,15.2,19.5,-8.0,12.6,23.9,
             6.1,24.1,14.5,5.3,19.1,-8.2,25.0,18.6,14.8,19.7),
    stringsAsFactors = FALSE
)
cntry_data <- merge(cntry_data, geo, by = "cntry", all.x = TRUE)

#' -------------------------------------------------------------------
#' ## 2c. Hofstede UAI [ESTENSIONE]
#' -------------------------------------------------------------------
hof <- read.csv(file.path(CONTEXTDATA_DIR, "hofstede_country_scores.csv"),
                stringsAsFactors = FALSE)

# Map country names to ISO2
hof_map <- c(Austria="AT",Belgium="BE",Bulgaria="BG",Croatia="HR",
             `Czech republic`="CZ",Denmark="DK",Estonia="EE",
             Finland="FI",France="FR",Germany="DE",Greece="GR",
             Hungary="HU",Ireland="IE",Italy="IT",Latvia="LV",
             Lithuania="LT",Luxembourg="LU",Malta="MT",Netherlands="NL",
             Poland="PL",Portugal="PT",Romania="RO",Slovakia="SK",
             Slovenia="SI",Spain="ES",Sweden="SE")
hof$cntry <- hof_map[hof$country]
hof <- hof[!is.na(hof$cntry), c("cntry", "uai")]
names(hof)[2] <- "UAI"

# Cyprus manca nel file Hofstede standard — UAI=65 da website
if (!"CY" %in% hof$cntry) {
    hof <- rbind(hof, data.frame(cntry = "CY", UAI = 65))
}

cntry_data <- merge(cntry_data, hof, by = "cntry", all.x = TRUE)

#' **Create numeric country identifier** [G&A]
cntry_data$cid <- recodeVar(cntry_data$cntry, isocntry, cid_seq, 
                             default = NA)
cntry_data$cid <- as.numeric(cntry_data$cid)

#' **Reshape to long format** [G&A + ESTENSIONE: 4 wave instead of 3]
cntry_long <- reshape(cntry_data, 
                      varying = list(
                          c("AGEOLD12",  "AGEOLD14",  "AGEOLD17",  "AGEOLD24"),
                          c("TECHEXP12", "TECHEXP14", "TECHEXP17", "TECHEXP24"),
                          c("INVEST12",  "INVEST14",  "INVEST17",  "INVEST24"),
                          c("UNEMP12",   "UNEMP14",   "UNEMP17",   "UNEMP24")),
                      v.names = c("AGEOLD", "TECHEXP", "INVEST", "UNEMP"),
                      timevar = "wave",
                      idvar = "cid",
                      drop = c("RESEXP12","RESEXP14","RESEXP17","RESEXP24",
                              "RESEAR12","RESEAR14","RESEAR17","RESEAR24","RESEAR22","RESEXP22","AGEOLD22","TECHEXP22","UNEMP22"),
                      direction = "long")
# UAI, LAT, LONG sono time-invariant → già nel long format




#' ===================================================================
#' # 3. Recode individual-level variables
#' ===================================================================

#' -------------------------------------------------------------------
#' ## Wave 1: 2012 [G&A]
#' -------------------------------------------------------------------
dat1$wave <- 1

dat1$cntry <- recodeVar(trimws(dat1$isocntry), 
                        c("DE-E", "DE-W", "GB-GBN", "GB-NIR"),
                        c("DE", "DE", "GB", "GB"))
dat1$cid <- as.numeric(recodeVar(dat1$cntry, isocntry, cid_seq, default = NA))

# Attitudes [G&A]
dat1$rob1  <- recodeVar(dat1$qa4,   1:4, 3:0, default = NA)
dat1$rob2  <- recodeVar(dat1$qa5_1, 1:4, 3:0, default = NA)
dat1$rob3  <- recodeVar(dat1$qa5_3, 1:4, 3:0, default = NA)
dat1$feel1 <- recodeVar(dat1$qa8_1, 1:10, 0:9, default = NA)
dat1$feel2 <- recodeVar(dat1$qa8_3, 1:10, 0:9, default = NA)
dat1$feel3 <- NA
dat1$feel4 <- NA

# Demographics [G&A]
dat1$married <- recodeVar(dat1$d7r2, 1:5, 1:5, default = NA) 
dat1$educ    <- recodeVar(dat1$d8r1, 1:11, c(1:9, 9, 0), default = NA) 
dat1$sex     <- recodeVar(dat1$d10, 1:2, 0:1, default = NA)
dat1$age     <- recodeVar(dat1$d11, 15:99, 15:99, default = NA)
dat1$empl    <- recodeVar(dat1$d15a, 1:18, 1:18, default = NA)

# Weights [G&A]
dat1$wgt1 <- dat1$w1
dat1$wgt2 <- dat1$w22


#' -------------------------------------------------------------------
#' ## Wave 2: 2014 [G&A]
#' -------------------------------------------------------------------
dat2$wave <- 2

dat2$cntry <- recodeVar(trimws(dat2$isocntry), 
                        c("DE-E", "DE-W", "GB-GBN", "GB-NIR"),
                        c("DE", "DE", "GB", "GB"))
dat2$cid <- as.numeric(recodeVar(dat2$cntry, isocntry, cid_seq, default = NA))

dat2$rob1  <- recodeVar(dat2$qa4,   1:4, 3:0, default = NA)
dat2$rob2  <- recodeVar(dat2$qa6_1, 1:4, 3:0, default = NA)
dat2$rob3  <- recodeVar(dat2$qa6_3, 1:4, 3:0, default = NA)
dat2$feel1 <- recodeVar(dat2$qa7_1, 1:10, 0:9, default = NA)
dat2$feel2 <- recodeVar(dat2$qa7_2, 1:10, 0:9, default = NA)
dat2$feel3 <- recodeVar(dat2$qa7_4, 1:10, 0:9, default = NA)
dat2$feel4 <- recodeVar(dat2$qa8_1, 1:10, 0:9, default = NA)

dat2$married <- recodeVar(dat2$d7r2, 1:5, 1:5, default = NA)
dat2$educ    <- recodeVar(dat2$d8r1, 1:11, c(1:9, 9, 0), default = NA)
dat2$sex     <- recodeVar(dat2$d10, 1:2, 0:1, default = NA)
dat2$age     <- recodeVar(dat2$d11, 15:99, 15:99, default = NA)
dat2$empl    <- recodeVar(dat2$d15a, 1:18, 1:18, default = NA)

dat2$wgt1 <- dat2$w1
dat2$wgt2 <- dat2$w22


#' -------------------------------------------------------------------
#' ## Wave 3: 2017 [G&A]
#' -------------------------------------------------------------------
dat3$wave <- 3

dat3$cntry <- recodeVar(trimws(dat3$isocntry), 
                        c("DE-E", "DE-W", "GB-GBN", "GB-NIR"),
                        c("DE", "DE", "GB", "GB"))
dat3$cid <- as.numeric(recodeVar(dat3$cntry, isocntry, cid_seq, default = NA))

dat3$rob1  <- recodeVar(dat3$qd10,   1:4, 3:0, default = NA) 
dat3$rob2 <- recodeVar(dat3$qd12_2, 1:4, 3:0, default = NA)  # help people
dat3$rob3 <- recodeVar(dat3$qd12_4, 1:4, 3:0, default = NA)  # do hard/dangerous jobs
dat3$feel1 <- recodeVar(dat3$qd13_1, 1:10, 0:9, default = NA) 
dat3$feel2 <- recodeVar(dat3$qd13_2, 1:10, 0:9, default = NA) 
dat3$feel3 <- recodeVar(dat3$qd13_3, 1:10, 0:9, default = NA) 
dat3$feel4 <- recodeVar(dat3$qd13_5, 1:10, 0:9, default = NA) 

dat3$married <- recodeVar(dat3$d7r2, 1:5, 1:5, default = NA)
dat3$educ    <- recodeVar(dat3$d8r1, 1:11, c(1:9, 9, 0), default = NA)
dat3$sex     <- recodeVar(dat3$d10, 1:2, 0:1, default = NA)
dat3$age     <- recodeVar(dat3$d11, 15:99, 15:99, default = NA)
dat3$empl    <- recodeVar(dat3$d15a, 1:18, 1:18, default = NA)

dat3$wgt1 <- dat3$w1
dat3$wgt2 <- dat3$w22


#' -------------------------------------------------------------------
#' ## Wave 4: 2024 [ESTENSIONE]
#' -------------------------------------------------------------------
#' NOTA: La wave 2024 presenta differenze rispetto alle precedenti:
#'   - "robots and AI" anziché solo "robots"
#'   - rob3 (item "necessary"): da "hard/dangerous" a "boring/repetitive"
#'   - UK assente (post-Brexit)
#'   - Genere: include opzione "non-binary" (N=49)
#'   - d8r1 potrebbe non esistere → usare d8 direttamente
#'
dat4$wave <- 4

dat4$cntry <- recodeVar(trimws(dat4$isocntry), 
                        c("DE-E", "DE-W"),
                        c("DE", "DE"))
dat4$cid <- as.numeric(recodeVar(dat4$cntry, isocntry, cid_seq, default = NA))

#' **Attitudes** [ESTENSIONE]
#' rob1 = qb5:  "How positively or negatively do you perceive the use of 
#'               robots and AI in the workplace?"
#'               1=Very positively, 2=Fairly positively, 
#'               3=Fairly negatively, 4=Very negatively, 999=DK
#' rob2 = qb6_2: "Robots and AI are a good thing for society, because they 
#'                help people do their jobs or carry out daily tasks at home"
#'                1=Totally agree → 4=Totally disagree, 999=DK
#' rob3 = qb6_4: "AI is necessary as it can do jobs that are seen as boring
#'                or repetitive"  ⚠ FORMULAZIONE CAMBIATA
#'                1=Totally agree → 4=Totally disagree, 999=DK
dat4$rob1 <- recodeVar(dat4$qb5,   1:4, 3:0, default = NA)
dat4$rob2 <- recodeVar(dat4$qb6_2, 1:4, 3:0, default = NA)
dat4$rob3 <- recodeVar(dat4$qb6_4, 1:4, 3:0, default = NA)

#' Items feel non presenti nella wave 2024 con scala 1-10 comparabile.
#' qb8 usa scala 4-punti (very positively → very negatively) per task
#' specifici, ma non è direttamente comparabile con le scale 1-10 di feel.
dat4$feel1 <- NA
dat4$feel2 <- NA
dat4$feel3 <- NA
dat4$feel4 <- NA

#' **Demographics** [ESTENSIONE]
dat4$educ <- recodeVar(dat4$d8r1, 1:11, c(1:9, 9, 0), default = NA)
dat4$married <- recodeVar(dat4$d7r, 1:5, 1:5, default = NA)
dat4$sex  <- recodeVar(dat4$d10, 1:2, 0:1, default = NA)
dat4$age  <- suppressWarnings(as.numeric(dat4$d11))
dat4$age[dat4$age < 15 | dat4$age > 99] <- NA
dat4$empl <- recodeVar(dat4$d15a, 1:18, 1:18, default = NA)

dat4$wgt1 <- dat4$w1
dat4$wgt2 <- dat4$w22

#' **Item aggiuntivi 2024** [ESTENSIONE — per Focus Italia]
dat4$steal_jobs   <- recodeVar(dat4$qb6_5, 1:4, 3:0, default = NA)
dat4$careful_mgmt <- recodeVar(dat4$qb6_3, 1:4, 3:0, default = NA)




#' ===================================================================
#' # 4. Merge data
#' ===================================================================

#' [G&A + ESTENSIONE: aggiunta wave 4, escluso UK e HR wave 1]
items <- c("cid", "wave", 
           paste0("rob", 1:3), paste0("feel", 1:4),
           "married", "age", "sex", "educ", "empl", 
           "wgt1", "wgt2")

dat <- suppressWarnings(
    bind_rows(dat1[, items], dat2[, items]) %>%
    bind_rows(dat3[, items]) %>%
    bind_rows(dat4[, items]) %>%
    left_join(cntry_long, by = c("cid", "wave")) %>%
    filter(!is.na(cid)) %>%           # [ESTENSIONE] rimuovi UK (cid = NA)
    filter(!(cntry == "HR" & wave == 1))  # [G&A] HR non incluso nella wave 1
)
rm(dat1, dat2, dat3, dat4, items)




#' ===================================================================
#' # 5. Recode derived variables
#' ===================================================================

#' **Job type** [G&A]
dat$white <- recodeVar(dat$empl, 
                       1:18, 
                       c(3, 3, 3, 3, 2, 2, 1, 2, 
                         rep(1, 6), 2, 2, 2, 2),
                       default = NA) 

#' **General appraisal of robots** [G&A]
dat$rob <- rowSums(dat[, paste0("rob", 1:3)])

#' **Composite score a 2 item** [ESTENSIONE — robustezza per cambio item 2024]
dat$rob2item <- rowSums(dat[, paste0("rob", 1:2)])

#' **Create unique person identifier** [G&A]
dat$pid <- seq_len(nrow(dat))

#' **Report campione**
cat("\n=== CAMPIONE ===\n")
cat("Righe totali:", nrow(dat), "\n")
cat("Per wave:\n")
print(table(dat$wave))
cat("\nPer paese (wave 4):\n")
print(sort(table(dat$cntry[dat$wave == 4])))
cat("\nN Italia per wave:\n")
print(table(dat$wave[dat$cntry == "IT"]))




#' ===================================================================
#' # 6. Multiple imputation [G&A]
#' ===================================================================
#' G&A usano MICE con m=20 imputazioni e metodo CART.
#' Decommentare questa sezione per replicare esattamente.
#' Se si preferisce listwise deletion, saltare al punto 7.

#' **Select variables for imputation** [G&A]
items_imp <- c("pid", "cid", 
               paste0("rob", 1:3), "rob", 
               paste0("feel", 1:4), 
               "sex", "age", "educ", 
               "white", "wave", "wgt1", "wgt2")
d <- dat[, items_imp]
d$white <- as.factor(d$white)
d$sex   <- as.factor(d$sex)

#' **Setup imputation** [G&A]
ini <- mice(d[, items_imp], maxit = 0, seed = 2352398)
cat("\nPercentuale missing:\n")
print(round(ini$nmis / nrow(d), 3))

#' **Predictor matrix** [G&A]
ini$predictorMatrix[c("pid", "cid", "wave", "wgt1", "wgt2"), items_imp] <- 0
ini$predictorMatrix[items_imp, c("pid", "cid", "wave", "wgt1", "wgt2")] <- 0

#' **Imputation method** [G&A]
ini$method[c(paste0("rob", 1:3), "rob", 
             paste0("feel", 1:4), "educ")] <- "cart"

#' **Run imputation** [G&A]
cat("\nEsecuzione MICE (m=20, potrebbe richiedere alcuni minuti)...\n")
dati_mice <- mice(d[, items_imp],
                  m               = 20,
                  print           = TRUE,
                  seed            = 42,
                  predictorMatrix = ini$predictorMatrix,
                  method          = ini$method,
                  visitSequence   = ini$visitSequence)
rm(ini, d)

#' **Convert to list of imputed datasets** [G&A + ESTENSIONE]
dati <- list()
for (i in seq_len(dati_mice$m)) {
    dati[[i]] <- complete(dati_mice, i)
    # [G&A] feel3, feel4 non esistono in wave 1
    dati[[i]]$feel3[dati[[i]]$wave == 1] <- NA
    dati[[i]]$feel4[dati[[i]]$wave == 1] <- NA
    # [ESTENSIONE] feel1-4 non esistono (come scala 1-10) in wave 4
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

save(dat, dati, dati_mice, file = file.path(DATA_DIR, "dat.Rdata"))
cat("\n✓ Salvato:", file.path(DATA_DIR, "dat.Rdata"), "\n")
cat("  dat:       ", nrow(dat), "righe (dati grezzi)\n")
cat("  dati:      ", length(dati), "dataset imputati\n")
cat("  dati_mice: ", "oggetto mice originale\n")




#' ===================================================================
#' # 8. Quick validation
#' ===================================================================

cat("\n=== VALIDAZIONE ===\n")

#' **Composite score medio per wave** (non ponderato)
cat("\nComposite score medio per wave (non ponderato):\n")
for (w in 1:4) {
    m <- mean(dat$rob[dat$wave == w], na.rm = TRUE)
    n <- sum(!is.na(dat$rob[dat$wave == w]))
    cat(sprintf("  Wave %d: M=%.2f, N=%d\n", w, m, n))
}

#' **Italia vs EU**
cat("\nItalia vs EU (composite score non ponderato):\n")
for (w in 1:4) {
    it <- mean(dat$rob[dat$wave == w & dat$cntry == "IT"], na.rm = TRUE)
    eu <- mean(dat$rob[dat$wave == w], na.rm = TRUE)
    cat(sprintf("  Wave %d: IT=%.2f, EU=%.2f, diff=%+.2f\n", w, it, eu, it - eu))
}

#' **UAI presente?**
cat("\nUAI sample (prime 5 righe wave 4):\n")
print(head(dat[dat$wave == 4, c("cntry", "UAI", "AGEOLD", "TECHEXP", "INVEST", "UNEMP", "LAT", "LONG")], 5))

cat("\n✓ Script completato. Procedere con 2_1_Descriptives.R\n")
