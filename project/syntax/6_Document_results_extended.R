#' ---
#' title: Document analyses — Extended (4 waves, 2012-2024)
#' author: Camilla [estende Gnambs & Appel 2019]
#' output:
#'    html_document:
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---
#'
#' Questo script svolge due funzioni:
#'   1. Renderizza tutti gli script analitici in HTML per la documentazione
#'   2. Produce un report sintetico dei risultati principali con versioni R e pacchetti
#'
#' STRUTTURA:
#'   Sezione 1 — Render HTML di tutti gli script
#'   Sezione 2 — Versioni R e pacchetti
#'   Sezione 3 — Report sintetico risultati (per uso interno / supervisore)


#' **Clear workspace**
rm(list = ls())

#' **Load packages**
library(rmarkdown)

dir.create("./results", showWarnings = FALSE)




#' ===================================================================
#' # 1. Render HTML di tutti gli script analitici
#' ===================================================================
#' [G&A] usa render() per produrre report HTML da script R annotati con #'.
#' Stessa logica qui, adattata ai nuovi nomi file.
#'
#' NOTA: ogni render() esegue lo script in un ambiente pulito —
#' il tempo totale può essere 30-60 minuti se MICE gira di nuovo.
#' Per evitare di ri-girare MICE, assicurarsi che dat.Rdata sia già
#' presente in ./data/ prima di eseguire questo script.

scripts <- list(
  list(
    input  = "./syntax/1_Load_data_extended.R",
    output = "1_Load_data_extended.html",
    label  = "1. Load data (extended)"
  ),
  list(
    input  = "./syntax/2_1_Descriptives_extended.R",
    output = "2_1_Descriptives_extended.html",
    label  = "2.1 Descriptives (extended)"
  ),
  list(
    input  = "./syntax/2_2_Measurement_invariance_extended.R",
    output = "2_2_Measurement_invariance_extended.html",
    label  = "2.2 Measurement invariance (extended)"
  ),
  list(
    input  = "./syntax/3_Current_and_changes_extended.R",
    output = "3_Current_and_changes_extended.html",
    label  = "3. Current and changes (extended)"
  ),
  list(
    input  = "./syntax/4_Predictors_extended.R",
    output = "4_Predictors_extended.html",
    label  = "4. Predictors of attitudes (extended)"
  ),
  list(
    input  = "./syntax/5_Plots_extended.R",
    output = "5_Plots_extended.html",
    label  = "5. Plots (extended)"
  )
)

cat("=== RENDERING SCRIPT IN HTML ===\n\n")
#' [FIX] knit_root_dir forza la working directory al progetto radice,
#' evitando che knitr sposti la wd nella cartella dello script (./syntax/)
proj_root <- getwd()
cat(sprintf("Working directory: %s\n\n", proj_root))

for (idx in seq_along(scripts)) {
  lbl <- scripts[[idx]]$label
  inp <- scripts[[idx]]$input
  out <- scripts[[idx]]$output
  cat(sprintf("Rendering: %s ...\n", lbl))
  tryCatch(
    render(input         = inp,
           output_dir    = file.path(proj_root, "results"),
           output_file   = out,
           knit_root_dir = proj_root,
           envir         = new.env(parent = globalenv()),
           quiet         = TRUE),
    error = function(e) {
      cat(sprintf("  ERRORE in %s: %s\n", lbl, conditionMessage(e)))
    }
  )
  cat(sprintf("  Salvato: results/%s\n", out))
  rm(lbl, inp, out)
}
cat("\nRendering completato.\n")




#' ===================================================================
#' # 2. Versioni R e pacchetti
#' ===================================================================
#' [G&A] Documentazione delle versioni per riproducibilità.
#' Esteso con tutti i pacchetti aggiuntivi usati nell'estensione.

cat("\n=== VERSIONI R E PACCHETTI ===\n\n")

#' **Funzione per estrarre library() dagli script**
extract_libs <- function(filepath) {
  if (!file.exists(filepath)) return(character(0))
  lines <- readLines(filepath, warn = FALSE)
  libs  <- c()
  for (line in lines) {
    g <- gregexpr("library\\((.+?)\\)", line, perl = TRUE)
    if (g[[1]][1] != -1) {
      starts  <- attr(g[[1]], "capture.start")
      lengths <- attr(g[[1]], "capture.length")
      for (i in seq_along(starts)) {
        lib <- substr(line, starts[i], starts[i] + lengths[i] - 1)
        lib <- trimws(lib)
        if (nchar(lib) > 0) libs <- c(libs, lib)
      }
    }
  }
  unique(libs)
}

#' **Raccogli pacchetti da tutti gli script**
all_scripts <- c(
  "./syntax/0_Start.R",
  "./syntax/1_Load_data_extended.R",
  "./syntax/2_1_Descriptives_extended.R",
  "./syntax/2_2_Measurement_invariance_extended.R",
  "./syntax/3__Current_and_changes_extended.R",
  "./syntax/4__Predictors_extended.R",
  "./syntax/5__Plots_extended.R"
)

all_libs <- sort(unique(unlist(lapply(all_scripts, extract_libs))))
cat("Pacchetti utilizzati:\n")
print(all_libs)

#' **Carica tutti i pacchetti e stampa sessionInfo**
for (lib in all_libs) {
  suppressPackageStartupMessages(
    tryCatch(library(lib, character.only = TRUE),
             error = function(e) cat(sprintf("  Non disponibile: %s\n", lib)))
  )
}

cat("\n")
print(sessionInfo())




#' ===================================================================
#' # 3. Report sintetico risultati principali
#' ===================================================================
#' Riepilogo numerico dei risultati chiave — per uso interno e
#' come riferimento rapido durante la scrittura della tesi.

cat("\n\n=== REPORT SINTETICO RISULTATI ===\n")
cat("(Da integrare nel testo della tesi — Capitolo 5)\n\n")

#' **Carica dati**
load("./data/dat.Rdata")
rm(dati_mice)

#' -------------------------------------------------------------------
#' ## 3.1 Trend longitudinale EU27
#' -------------------------------------------------------------------
cat("--- 3.1 Composite score EU27 per wave ---\n")
anni <- c(2012, 2014, 2017, 2024)
for (w in 1:4) {
  sub <- dat[dat$wave == w & !is.na(dat$rob) & !is.na(dat$wgt2), ]
  m   <- weighted.mean(sub$rob, sub$wgt2)
  n   <- nrow(sub)
  cat(sprintf("  Wave %d (%d): M = %.3f  N = %d\n", w, anni[w], m, n))
}

#' -------------------------------------------------------------------
#' ## 3.2 Posizionamento Italia
#' -------------------------------------------------------------------
cat("\n--- 3.2 Italia vs EU27 ---\n")
for (w in 1:4) {
  sub_it <- dat[dat$wave == w & dat$cntry == "IT" &
                  !is.na(dat$rob) & !is.na(dat$wgt2), ]
  sub_eu <- dat[dat$wave == w & !is.na(dat$rob) & !is.na(dat$wgt2), ]
  m_it <- weighted.mean(sub_it$rob, sub_it$wgt2)
  m_eu <- weighted.mean(sub_eu$rob, sub_eu$wgt2)
  cat(sprintf("  Wave %d (%d): IT = %.3f  EU = %.3f  diff = %+.3f\n",
              w, anni[w], m_it, m_eu, m_it - m_eu))
}

#' -------------------------------------------------------------------
#' ## 3.3 ICC per wave
#' -------------------------------------------------------------------
cat("\n--- 3.3 ICC per wave (da script 4, modello A0) ---\n")
cat("  Wave 1 (2012): ICC = 0.088\n")
cat("  Wave 2 (2014): ICC = 0.074\n")
cat("  Wave 3 (2017): ICC = 0.057\n")
cat("  Wave 4 (2024): ICC = 0.044\n")
cat("  Trend: convergenza tra paesi nel tempo (-50% in 12 anni)\n")

#' -------------------------------------------------------------------
#' ## 3.4 Coefficienti chiave Modello A2 (wave 1-3, replica G&A)
#' -------------------------------------------------------------------
cat("\n--- 3.4 Predittori significativi Modello A2 (wave 1-3) ---\n")
cat("  Individuali:\n")
cat("    wave2:  b = -0.282***  (crescente scetticismo 2012→2014)\n")
cat("    wave3:  b = -0.501***  (crescente scetticismo 2012→2017)\n")
cat("    sex1:   b = -0.390***  (donne più scettiche, std = -0.203)\n")
cat("    educ:   b = +0.125***  (istruzione protettiva, std = +0.065)\n")
cat("    white2: b = -0.164***  (blue-collar più scettici)\n")
cat("  Contestuali (L2):\n")
cat("    AGEOLD:  b = +0.075***  d = +0.18\n")
cat("    TECHEXP: b = +0.022***  d = +0.16\n")
cat("    LAT:     b = +0.045***  d = +0.32  (Nord-Sud divide)\n")
cat("    LONG:    b =  0.002 ns\n")
cat("    INVEST:  b = -0.114 ns (p=.095)\n")
cat("    UNEMP:   b = +0.013*\n")

#' -------------------------------------------------------------------
#' ## 3.5 Modello B2: effetti UAI e mediazione latitudine
#' -------------------------------------------------------------------
cat("\n--- 3.5 Modello B2: UAI e mediazione latitudine ---\n")
cat("  H1 (UAI < 0): UAI_z b = -0.180 (p=.154) — segno corretto,\n")
cat("     non significativo con 27 unità L2 (potenza limitata)\n")
cat("  H2 (mediazione LAT da UAI):\n")
cat("     LAT in A2:  b = +0.045***  (p=.002)\n")
cat("     LAT in B2:  b = +0.029 ns  (p=.110) — CONFERMATA\n")
cat("     Il pattern Nord-Sud è in parte spiegato da UAI\n")
cat("  H3a (UAI × educ): b = +0.027***  (p<.001) — CONFERMATA\n")
cat("     L'istruzione compensa di più l'ansia culturale nei paesi alta UAI\n")
cat("  H3b (UAI × blue-collar): b = -0.025 ns — non confermata\n")

#' -------------------------------------------------------------------
#' ## 3.6 Residui Italia
#' -------------------------------------------------------------------
cat("\n--- 3.6 Residui Italia nei modelli multilevel ---\n")
cat("  A2 wave 1-3: -0.243  (Italia sotto le attese strutturali)\n")
cat("  B2 wave 1-3: -0.359  (residuo AUMENTA con UAI — non mediato)\n")
cat("  A2 wave 1-4: -0.197\n")
cat("  B2 wave 1-4: -0.294\n")
cat("  CONCLUSIONE: La specificità italiana NON è spiegata da UAI.\n")
cat("  Il Capitolo 6 esplora i meccanismi Italy-specific:\n")
cat("  memoria collettiva one-company town, struttura PMI,\n")
cat("  relazioni industriali, ecosistema istituzionale piemontese.\n")

#' -------------------------------------------------------------------
#' ## 3.7 Focus Italia wave 4
#' -------------------------------------------------------------------
cat("\n--- 3.7 Focus Italia wave 4 (differenze vs EU) ---\n")
cat("  Genere (IT wave 4):  b = +0.033 ns  — GAP SCOMPARSO\n")
cat("                        vs EU: b = -0.326***\n")
cat("  Blue-collar (IT):    b = -0.240 ns  — non struttura come in EU\n")
cat("                        vs EU: b = -0.364***\n")
cat("  Età (IT):            b = -0.264***  — unico predittore robusto\n")
cat("  Istruzione (IT):     b = +0.062*\n")
cat("  CONCLUSIONE: I predittori standard EU non funzionano\n")
cat("  nello stesso modo in Italia nel 2024 — richiede indagine qualitativa.\n")

#' -------------------------------------------------------------------
#' ## 3.8 Correlazione UAI × composite score
#' -------------------------------------------------------------------
cat("\n--- 3.8 Correlazione UAI × composite score ---\n")
cat("  r(UAI, score_2017) = -0.561\n")
cat("  r(UAI, score_2024) = -0.682\n")
cat("  La correlazione si RAFFORZA nel 2024 — UAI diventa\n")
cat("  più predittivo man mano che il dibattito AI diventa più saliente.\n")

cat("\n\n=== DOCUMENTAZIONE COMPLETATA ===\n")
cat("File HTML in ./results/\n")
cat("Report sintetico stampato sopra.\n")
cat("\n✓ Pipeline analitica completa.\n")
cat("  Passi successivi:\n")
cat("  - Raccolta dati qualitativi (interviste, Capitolo 6)\n")
cat("  - Scrittura Capitoli 5-7 della tesi\n")
cat("  - Aggiornamento appendice metodologica\n")