#' ---
#' title: Measurement invariance — Extended (4 waves, 2012-2024)
#' author: Camilla [estende Gnambs & Appel 2019 — lavaan invece di Mplus]
#' output:
#'    html_document:
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---
#'
#' **NOTA METODOLOGICA**
#' G&A usano Mplus con MplusAutomation per il test di invarianza.
#' Questo script replica la stessa sequenza di modelli (configurale →
#' metrico → scalare) usando lavaan + semTools in R, con stimatore
#' WLSMV appropriato per item ordinali a 4 categorie.
#'
#' Struttura:
#'   Sezione 1 — Invarianza wave 1-3 a 3 item (replica G&A)
#'   Sezione 2 — Comparabilità wave 1-4 a 2 item (correlazioni policoriche)
#'   Sezione 3 — Significatività pratica non-invarianza (replica G&A)
#'   Sezione 4 — Riepilogo e decisione metodologica


#' **Clear workspace**
rm(list = ls())

#' **Load packages**
library(lavaan)
library(semTools)
library(polycor)     # correlazioni policoriche per sezione 2
library(doBy)
source("./syntax/0_Start.R")

#' **Load data**
load("./data/dat.Rdata")
rm(dati_mice)

#' **Prepara item ordinali e subset**
dat$rob1 <- ordered(dat$rob1)
dat$rob2 <- ordered(dat$rob2)
dat$rob3 <- ordered(dat$rob3)

# Wave 1-3: 3 item (replica G&A)
d123 <- dat[dat$wave %in% 1:3, ]
d123$wave_f <- factor(d123$wave, labels = c("w2012", "w2014", "w2017"))

# Wave 1-4: 2 item (estensione — rob3 non comparabile in wave 4)
d1234 <- dat[dat$wave %in% 1:4, ]
d1234$wave_f <- factor(d1234$wave,
                       labels = c("w2012", "w2014", "w2017", "w2024"))




#' ===================================================================
#' # 1. Measurement invariance across waves 1-3 (replica G&A)
#' ===================================================================
#' Fattore unico F su rob1, rob2, rob3
#' Stimatore WLSMV, 3 gruppi (wave), sequenza configurale -> metrico -> scalare

cat("\n=== INVARIANZA DI MISURA: WAVE 1-3 (3 item) ===\n")
cat("Replica di Gnambs & Appel (2019)\n\n")

model_3item <- '
  F =~ rob1 + rob2 + rob3
'

#' -------------------------------------------------------------------
#' ## 1a. Modello configurale
#' -------------------------------------------------------------------
cat("--- Modello 1: Configurale ---\n")
fit_conf_123 <- cfa(model_3item,
                    data        = d123,
                    group       = "wave_f",
                    estimator   = "WLSMV",
                    ordered     = c("rob1", "rob2", "rob3"),
                    group.equal = character(0))

fitMeasures(fit_conf_123, c("cfi", "tli", "rmsea", "srmr"))


#' -------------------------------------------------------------------
#' ## 1b. Modello metrico (loadings vincolati)
#' -------------------------------------------------------------------
cat("\n--- Modello 2: Metrico ---\n")
fit_metr_123 <- cfa(model_3item,
                    data        = d123,
                    group       = "wave_f",
                    estimator   = "WLSMV",
                    ordered     = c("rob1", "rob2", "rob3"),
                    group.equal = "loadings")

fitMeasures(fit_metr_123, c("cfi", "tli", "rmsea", "srmr"))


#' -------------------------------------------------------------------
#' ## 1c. Modello scalare (loadings + thresholds vincolati)
#' -------------------------------------------------------------------
cat("\n--- Modello 3: Scalare ---\n")
fit_scal_123 <- cfa(model_3item,
                    data        = d123,
                    group       = "wave_f",
                    estimator   = "WLSMV",
                    ordered     = c("rob1", "rob2", "rob3"),
                    group.equal = c("loadings", "thresholds"))

fitMeasures(fit_scal_123, c("cfi", "tli", "rmsea", "srmr"))


#' -------------------------------------------------------------------
#' ## 1d. Tabella riepilogativa con delta fit
#' -------------------------------------------------------------------
cat("\n--- Confronto modelli (wave 1-3) ---\n")
#' Criterio: |dCFI| < .010 e |dRMSEA| < .015 (Cheung & Rensvold 2002)
#' Con N ~75.000 il test LRT e' quasi sempre significativo -> usare dCFI

cfi_123   <- sapply(list(fit_conf_123, fit_metr_123, fit_scal_123),
                    function(x) fitMeasures(x, "cfi"))
rmsea_123 <- sapply(list(fit_conf_123, fit_metr_123, fit_scal_123),
                    function(x) fitMeasures(x, "rmsea"))
tli_123   <- sapply(list(fit_conf_123, fit_metr_123, fit_scal_123),
                    function(x) fitMeasures(x, "tli"))
df_123    <- sapply(list(fit_conf_123, fit_metr_123, fit_scal_123),
                    function(x) fitMeasures(x, "df"))

tab_123 <- data.frame(
  Modello = c("Configurale", "Metrico", "Scalare"),
  df      = df_123,
  CFI     = round(cfi_123,   3),
  TLI     = round(tli_123,   3),
  RMSEA   = round(rmsea_123, 3),
  dCFI    = round(c(NA, diff(cfi_123)),   3),
  dRMSEA  = round(c(NA, diff(rmsea_123)), 3)
)
cat("\nRiepilogo fit indices (wave 1-3, 3 item):\n")
print(tab_123)




#' ===================================================================
#' # 2. Comparabilita' wave 1-4: correlazioni policoriche (2 item)
#' ===================================================================
#' Con soli 2 indicatori la CFA configurale ha df=0 e non e' identificata.
#' L'approccio corretto e' il confronto delle correlazioni policoriche
#' (equivalente funzionale del test di invarianza metrica per scale a
#' 2 item) e delle distribuzioni di risposta (invarianza scalare approx).
#' Riferimento: Raykov (2012), Psychological Assessment.

cat("\n\n=== COMPARABILITA' WAVE 1-4: 2 ITEM (rob1 + rob2) ===\n")
cat("[ESTENSIONE] CFA non identificata con 2 item (df=0 nel configurale).\n")
cat("Approccio: correlazioni policoriche + distribuzioni di risposta.\n\n")

#' -------------------------------------------------------------------
#' ## 2a. Correlazioni policoriche rob1 x rob2 per wave
#' -------------------------------------------------------------------
#' Invarianza metrica = stabilita' della correlazione (dr < .05)

cat("--- Correlazioni policoriche rob1 x rob2 per wave ---\n")
polychor_tab <- data.frame(wave  = integer(),
                           anno  = character(),
                           r_poly = numeric(),
                           SE    = numeric(),
                           N     = integer())

for (w in 1:4) {
  sub <- d1234[d1234$wave == w &
                 !is.na(d1234$rob1) & !is.na(d1234$rob2), ]
  pc  <- polychor(as.numeric(sub$rob1), as.numeric(sub$rob2),
                  std.err = TRUE)
  polychor_tab <- rbind(polychor_tab,
                        data.frame(wave   = w,
                                   anno   = c("2012","2014","2017","2024")[w],
                                   r_poly = round(pc$rho, 3),
                                   SE     = round(sqrt(pc$var[1,1]), 3),
                                   N      = nrow(sub)))
}
print(polychor_tab)
cat(sprintf("\n  Max dr tra wave adiacenti: %.3f\n",
            max(abs(diff(polychor_tab$r_poly)))))
cat("  Criterio: dr < .05 -> invarianza metrica supportata\n")


#' -------------------------------------------------------------------
#' ## 2b. Distribuzioni di risposta per wave
#' -------------------------------------------------------------------
cat("\n--- Distribuzioni risposta rob1 per wave (proporzioni) ---\n")
print(round(prop.table(
  table(as.numeric(d1234$rob1), d1234$wave_f), margin = 2), 3))

cat("\n--- Distribuzioni risposta rob2 per wave (proporzioni) ---\n")
print(round(prop.table(
  table(as.numeric(d1234$rob2), d1234$wave_f), margin = 2), 3))

cat("
  Interpretazione: proporzioni stabili (max D < .05) -> invarianza
  scalare approssimata supportata. Differenze sistematiche indicano
  shift nelle soglie di risposta tra wave (response style change).
")




#' ===================================================================
#' # 3. Significatività pratica della non-invarianza (replica G&A)
#' ===================================================================
#' G&A verificano che le differenze nelle probabilita' predette di
#' risposta tra modello configurale e scalare siano < .06.
#' Usa ThresholdProbability() definita in 0_Start.R

cat("\n\n=== SIGNIFICATIVITA' PRATICA NON-INVARIANZA (wave 1-3) ===\n")
cat("Replica di Gnambs & Appel (2019)\n\n")

#' Helper: estrai parametri da oggetto lavaan per un gruppo
extract_params_lavaan <- function(fit, group_idx, items) {
  
  params <- parameterestimates(fit, standardized = FALSE)
  
  # factor mean (0 nel gruppo di riferimento)
  F_mean <- params$est[params$lhs == "F" &
                         params$op  == "~1" &
                         params$group == group_idx]
  if (length(F_mean) == 0) F_mean <- 0
  
  # factor variance
  F_var <- params$est[params$lhs == "F" &
                        params$op  == "~~" &
                        params$rhs == "F" &
                        params$group == group_idx]
  if (length(F_var) == 0) F_var <- 1
  
  # loadings
  loadings <- sapply(items, function(it) {
    v <- params$est[params$lhs == "F" &
                      params$op  == "=~" &
                      params$rhs == it &
                      params$group == group_idx]
    if (length(v) == 0) NA else v[1]
  })
  
  # thresholds (3 soglie per item a 4 categorie)
  thresholds <- lapply(items, function(it) {
    th <- params$est[params$lhs == it &
                       params$op  == "|" &
                       params$group == group_idx]
    if (length(th) < 3) rep(NA, 3) else th[1:3]
  })
  th_mat <- do.call(rbind, thresholds)
  
  list(F_mean    = F_mean,
       F_var     = F_var,
       loadings  = loadings,
       thresholds = th_mat)
}

items_3 <- c("rob1", "rob2", "rob3")

for (g in 1:3) {
  wave_label <- c("2012", "2014", "2017")[g]
  cat(sprintf("Wave %s:\n", wave_label))
  
  p_conf <- tryCatch({
    pr <- extract_params_lavaan(fit_conf_123, g, items_3)
    ThresholdProbability(3, pr$loadings, pr$F_mean, pr$F_var,
                         rep(1, 3), 3, pr$thresholds)
  }, error = function(e) {
    cat("  [Errore estrazione configurale]\n"); NULL
  })
  
  p_scal <- tryCatch({
    pr <- extract_params_lavaan(fit_scal_123, g, items_3)
    ThresholdProbability(3, pr$loadings, pr$F_mean, pr$F_var,
                         rep(1, 3), 3, pr$thresholds)
  }, error = function(e) {
    cat("  [Errore estrazione scalare]\n"); NULL
  })
  
  if (!is.null(p_conf) && !is.null(p_scal)) {
    diff_mat <- round(p_conf - p_scal, 3)
    rownames(diff_mat) <- items_3
    cat("  Differenza probabilita' (Configurale - Scalare):\n")
    print(diff_mat)
    max_diff <- max(abs(diff_mat), na.rm = TRUE)
    cat(sprintf("  Max diff assoluta: %.3f  %s\n\n",
                max_diff,
                ifelse(max_diff < .06,
                       "OK: < .06 (non-invarianza non sostanziale)",
                       "ATTENZIONE: > .06 (da discutere)")))
  }
}




#' ===================================================================
#' # 4. Riepilogo e decisione metodologica
#' ===================================================================

cat("\n=== RIEPILOGO FINALE ===\n\n")

cat("--- Wave 1-3 (3 item, replica G&A) ---\n")
print(tab_123)

cat("\n--- Wave 1-4 (2 item, estensione) ---\n")
print(polychor_tab)

cat("
=== REGOLE DECISIONALI ===

Wave 1-3 (CFA):
  |dCFI|   < .010 -> invarianza supportata  (Cheung & Rensvold 2002)
  |dRMSEA| < .015 -> invarianza supportata
  Max Dprob < .06  -> non-invarianza non sostanzialmente rilevante (G&A)

Wave 1-4 (correlazioni policoriche):
  dr_poly  < .05  -> invarianza metrica approssimata supportata
  Proporzioni stabili -> invarianza scalare approssimata

=== DECISIONE PER L'ANALISI PRINCIPALE ===

  Se invarianza (almeno metrica) supportata:
    -> Usare composite rob (3 item) per wave 1-3 nell'analisi multilevel
    -> Usare rob2item (2 item) per confronti longitudinali con wave 4
    -> Documentare entrambe le scelte in appendice metodologica (Cap. 4.3.4)

  Se invarianza scalare non supportata (dCFI > .010):
    -> Confronti di correlazioni e regressioni restano legittimi
    -> Confronti di medie richiedono cautela: riportare come limite
    -> G&A procedono con invarianza approssimata: stessa scelta qui
")

cat("\n✓ Script 2_2 completato. Procedere con 3__Current_and_changes_extended.R\n")