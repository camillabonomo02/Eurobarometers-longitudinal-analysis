#' ---
#' title: Predictors of attitudes — Extended (4 waves, 2012-2024)
#' author: Camilla [estende Gnambs & Appel 2019]
#' output:
#'    html_document:
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---
#'
#' STRUTTURA SCRIPT:
#'   Sezione 1 — Recode variabili
#'   Sezione 2 — Modello A0: null model (ICC) per wave [G&A + estensione]
#'   Sezione 3 — Modello A1: predittori individuali [G&A + wave 4]
#'   Sezione 4 — Modello A2: predittori individuali + contestuali [G&A replica]
#'   Sezione 5 — Modello B2: A2 + UAI [ESTENSIONE — H1 e H2]
#'   Sezione 6 — Confronto A2 vs B2 e analisi residui Italia
#'   Sezione 7 — Feel items (replica G&A, wave 1-3)
#'   Sezione 8 — Focus Italia wave 4: regressione su sotto-campione
#'
#' IPOTESI TESTATE:
#'   H1: UAI negativamente associato all'accettazione della robotica
#'       (paesi ad alta uncertainty avoidance mostrano atteggiamenti
#'       piu' negativi, controllando per tutti i predittori G&A)
#'   H2 (mediazione): l'effetto della latitudine si riduce o diventa
#'       non significativo aggiungendo UAI — la latitudine era un proxy
#'       della dimensione culturale


#' **Clear workspace**
rm(list = ls())

#' **Load packages**
library(lme4)
library(lmerTest)   # p-values con approssimazione Satterthwaite
library(mitml)
library(weights)
source("./syntax/0_Start.R")

#' **Load data**
load("./data/dat.Rdata")
rm(dati_mice)




#' ===================================================================
#' # 1. Recode variabili [G&A + ESTENSIONE wave 4]
#' ===================================================================

dati <- within(dati, {
  white <- as.factor(white)    # 1=white-collar, 2=blue-collar, 3=non-employed
  sex   <- as.factor(sex)      # 0=uomo, 1=donna
  wave  <- as.factor(wave)     # wave 1 = riferimento
  age   <- scale(age,  scale = FALSE) / 10   # centrata, unita' = 10 anni
  educ  <- scale(educ, scale = FALSE)         # centrata
  # [ESTENSIONE] UAI standardizzato a livello paese (time-invariant)
  # z-score calcolato sui 27 valori unici
})

#' **Standardizza UAI** (una sola osservazione per paese)
uai_mean <- mean(unique(dati[[1]][, c("cid", "UAI")])$UAI, na.rm = TRUE)
uai_sd   <- sd(unique(dati[[1]][, c("cid", "UAI")])$UAI,   na.rm = TRUE)
dati <- lapply(dati, function(x) {
  x$UAI_z <- (x$UAI - uai_mean) / uai_sd
  x
})
dati <- as.mitml.list(dati)

cat(sprintf("UAI: M=%.1f SD=%.1f (N paesi=%d)\n",
            uai_mean, uai_sd,
            length(unique(dati[[1]]$UAI[!is.na(dati[[1]]$UAI)]))))
cat("Italia UAI_z:", round((75 - uai_mean) / uai_sd, 3), "\n\n")




#' ===================================================================
#' # 2. Modello A0: null model — ICC per wave [G&A + ESTENSIONE]
#' ===================================================================
#' Replica degli ICC gia' calcolati in script 3, qui come baseline
#' formale prima dei modelli esplicativi.

cat("\n=== MODELLO A0: NULL MODEL (ICC per wave) ===\n")

for (w in 1:4) {
  cat(sprintf("\nWave %d:\n", w))
  lmer.imp(rob ~ 1 + (1 | cid), data = dati, weights = "wgt2",
           subset = (dati[[1]]$wave == w))
}




#' ===================================================================
#' # 3. Modello A1: predittori individuali [G&A + wave 4]
#' ===================================================================
#' Replica esatta di G&A per wave 1-3, poi estesa a wave 1-4.
#' wave1 = riferimento; wave2, wave3, wave4 = contrasti rispetto a wave1.
#' white1 = riferimento (white-collar); white2 = blue-collar; white3 = non-employed
#' sex0 = riferimento (uomo); sex1 = donna

#' -------------------------------------------------------------------
#' ## 3a. Wave 1-3 (replica G&A — modello originale)
#' -------------------------------------------------------------------
cat("\n=== MODELLO A1: PREDITTORI INDIVIDUALI (wave 1-3) ===\n")
cat("Replica esatta di Gnambs & Appel (2019, Table 2)\n\n")

fit_A1_123 <- lmer.imp(
  rob ~ wave + sex + age + educ + white + (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE,
  subset  = (dati[[1]]$wave %in% c(1, 2, 3))
)


#' -------------------------------------------------------------------
#' ## 3b. Wave 1-4 (ESTENSIONE)
#' -------------------------------------------------------------------
cat("\n=== MODELLO A1: PREDITTORI INDIVIDUALI (wave 1-4) ===\n")
cat("[ESTENSIONE] Include wave 4 (2024)\n\n")

fit_A1_1234 <- lmer.imp(
  rob ~ wave + sex + age + educ + white + (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE
)




#' ===================================================================
#' # 4. Modello A2: predittori individuali + contestuali [G&A replica]
#' ===================================================================
#' Replica esatta di G&A (2019, Table 3): aggiunge AGEOLD, TECHEXP,
#' INVEST, UNEMP, LAT, LONG come predittori di Livello 2.
#' Nota: G&A usano wave 1-3. Qui si replica prima su wave 1-3,
#' poi si estende a wave 1-4.

#' -------------------------------------------------------------------
#' ## 4a. Wave 1-3 (replica G&A)
#' -------------------------------------------------------------------
cat("\n=== MODELLO A2: PREDITTORI L1+L2 (wave 1-3 — replica G&A) ===\n")

fit_A2_123 <- lmer.imp(
  rob ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
    (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE,
  subset  = (dati[[1]]$wave %in% c(1, 2, 3)),
  control = lmerControl(optimizer = "nloptwrap")
)

#' Effetti completamente standardizzati + conversione in d (come G&A)
cat("\n--- Coefficienti completamente standardizzati (predittori L2) ---\n")
fit_A2_123_std <- lmer.imp(
  rob ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
    (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = TRUE,
  subset  = (dati[[1]]$wave %in% c(1, 2, 3)),
  control = lmerControl(optimizer = "nloptwrap"),
  print   = FALSE
)
# Conversione beta standardizzati in d per predittori L2 (posizioni 9-14)
d_A2_123 <- round(2 * fit_A2_123_std$std[9:14] /
                    sqrt(1 - fit_A2_123_std$std[9:14]^2), 2)
cat("Cohen's d predittori L2 (AGEOLD, TECHEXP, INVEST, UNEMP, LAT, LONG):\n")
print(d_A2_123)
rm(fit_A2_123_std)


#' -------------------------------------------------------------------
#' ## 4b. Wave 1-4 (ESTENSIONE)
#' -------------------------------------------------------------------
cat("\n=== MODELLO A2: PREDITTORI L1+L2 (wave 1-4) ===\n")
cat("[ESTENSIONE]\n\n")

fit_A2_1234 <- lmer.imp(
  rob ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
    (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE,
  control = lmerControl(optimizer = "nloptwrap")
)




#' ===================================================================
#' # 5. Modello B2: A2 + UAI [ESTENSIONE — test H1 e H2]
#' ===================================================================
#' H1: UAI_z negativamente associato all'accettazione (beta < 0)
#' H2: l'effetto di LAT si riduce/scompare aggiungendo UAI
#'     (LAT come proxy culturale)
#'
#' Se H2 confermata: il pattern geografico Nord-Sud e' mediato
#' dalla dimensione culturale (uncertainty avoidance).

#' -------------------------------------------------------------------
#' ## 5a. Modello B2 su wave 1-3
#' -------------------------------------------------------------------
cat("\n=== MODELLO B2: A2 + UAI (wave 1-3) ===\n")
cat("[ESTENSIONE] Test H1 (UAI) e H2 (mediazione latitudine)\n\n")

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


#' -------------------------------------------------------------------
#' ## 5b. Modello B2 su wave 1-4
#' -------------------------------------------------------------------
cat("\n=== MODELLO B2: A2 + UAI (wave 1-4) ===\n")

fit_B2_1234 <- lmer.imp(
  rob ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG + UAI_z +
    (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE,
  control = lmerControl(optimizer = "nloptwrap")
)


#' -------------------------------------------------------------------
#' ## 5c. Interazioni cross-level [ESTENSIONE — test moderazione UAI]
#' -------------------------------------------------------------------
#' H3a: UAI_z modera l'effetto dell'istruzione?
#'      (l'istruzione compensa l'ansia culturale nei paesi alta UAI?)
#' H3b: UAI_z modera l'effetto del blue-collar?
#'      (l'effetto negativo del lavoro manuale e' amplificato in alta UAI?)

cat("\n=== MODELLO B3: INTERAZIONI CROSS-LEVEL (wave 1-4) ===\n")
cat("[ESTENSIONE] Test moderazione UAI x istruzione e UAI x blue-collar\n\n")

fit_B3_1234 <- lmer.imp(
  rob ~ wave + sex + age + educ + white +
    AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG + UAI_z +
    UAI_z:educ +         # H3a: UAI modera effetto istruzione
    UAI_z:white +        # H3b: UAI modera effetto occupazione
    (1 | cid),
  data    = dati,
  weights = "wgt2",
  stdy    = TRUE,
  stdx    = FALSE,
  control = lmerControl(optimizer = "nloptwrap")
)




#' ===================================================================
#' # 6. Confronto A2 vs B2 e analisi residui Italia
#' ===================================================================

cat("\n\n=== CONFRONTO A2 vs B2: VARIANZA SPIEGATA AL LIVELLO 2 ===\n")

#' Estrai varianze dei random effects per confronto
extract_var_components <- function(fit_list, label) {
  vars <- sapply(fit_list, function(f) {
    vc <- VarCorr(f)
    c(intercept = as.numeric(vc$cid),
      residual  = attr(vc, "sc")^2)
  })
  icc <- rowMeans(vars)["intercept"] /
    sum(rowMeans(vars))
  cat(sprintf("\n%s:\n", label))
  cat(sprintf("  Varianza intercetta (paese): %.4f\n",
              rowMeans(vars)["intercept"]))
  cat(sprintf("  Varianza residua:            %.4f\n",
              rowMeans(vars)["residual"]))
  cat(sprintf("  ICC:                         %.4f\n", icc))
  invisible(rowMeans(vars))
}

#' Confronto wave 1-3
cat("\n--- Wave 1-3 ---\n")
for (i in seq_along(dati)) {
  dati[[i]]$wave_num <- as.numeric(as.character(dati[[i]]$wave))
}

# Fit modelli base per estrazione varianze (senza imputation pooling)
# Usiamo il primo dataset imputato per confronto rapido
d1 <- dati[[1]]
d1_123 <- d1[d1$wave_num %in% 1:3, ]

# [FIX] Pesi normalizzati per evitare AIC/BIC = Inf
# (wgt2 ha scala assoluta che rende la log-likelihood numericamente instabile)
d1_123$wgt2_norm <- d1_123$wgt2 / mean(d1_123$wgt2, na.rm = TRUE)
d1$wgt2_norm     <- d1$wgt2     / mean(d1$wgt2,     na.rm = TRUE)

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

#' NOTA: AIC/BIC = Inf con lme4 su N molto grandi anche con pesi normalizzati
#' — problema noto di overflow nella log-likelihood con N > 50.000.
#' Confronto alternativo: riduzione varianza L2 (pseudo-R2 livello paese).
cat("\nConfronto varianza L2 A2 vs B2 (wave 1-3, primo dataset imputato):\n")
vc_A2 <- as.data.frame(VarCorr(m_A2))
vc_B2 <- as.data.frame(VarCorr(m_B2))
var_A2 <- vc_A2$vcov[vc_A2$grp == "cid"]
var_B2 <- vc_B2$vcov[vc_B2$grp == "cid"]
res_A2 <- vc_A2$vcov[vc_A2$grp == "Residual"]
res_B2 <- vc_B2$vcov[vc_B2$grp == "Residual"]
cat(sprintf("  Var intercetta A2: %.4f   ICC A2: %.4f\n",
            var_A2, var_A2 / (var_A2 + res_A2)))
cat(sprintf("  Var intercetta B2: %.4f   ICC B2: %.4f\n",
            var_B2, var_B2 / (var_B2 + res_B2)))
cat(sprintf("  Riduzione varianza L2 aggiungendo UAI: %.1f%%\n",
            (var_A2 - var_B2) / var_A2 * 100))


#' -------------------------------------------------------------------
#' ## 6a. Residui Italia — confronto A2 vs B2
#' -------------------------------------------------------------------
#' Se il residuo italiano si riduce passando da A2 a B2:
#'   -> UAI spiega parte della specificita' italiana
#' Se il residuo persiste anche in B2:
#'   -> c'e' qualcosa di specificamente italiano oltre la cultura Hofstede
#'      (memoria one-company town? struttura PMI? relazioni industriali?)
#'   -> il Capitolo 6 qualitativo esplora questi meccanismi

cat("\n=== RESIDUI ITALIA — CONFRONTO A2 vs B2 ===\n")

re_A2 <- ranef(m_A2)$cid
re_B2 <- ranef(m_B2)$cid

# Aggiungi cntry per identificazione
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

cat("\nRandom effects per paese (A2 vs B2, wave 1-3):\n")
re_compare[, c("re_A2", "re_B2", "change")] <-
  round(re_compare[, c("re_A2", "re_B2", "change")], 3)
print(re_compare)

# Italia
it_re <- re_compare[re_compare$cntry == "IT", ]
cat(sprintf("\n*** ITALIA ***\n"))
cat(sprintf("  Residuo Modello A2: %+.3f\n", it_re$re_A2))
cat(sprintf("  Residuo Modello B2: %+.3f\n", it_re$re_B2))
cat(sprintf("  Variazione:         %+.3f\n", it_re$change))
cat(sprintf("  Interpretazione: UAI spiega %.1f%% del residuo italiano\n",
            abs(it_re$change / it_re$re_A2) * 100))

#' Residui wave 1-4
cat("\n--- Residui wave 1-4 ---\n")
m_A2_4 <- lmer(rob ~ wave + sex + age + educ + white +
                 AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
                 (1 | cid),
               data    = d1,
               weights = wgt2_norm,
               REML    = FALSE,
               control = lmerControl(optimizer = "nloptwrap"))

m_B2_4 <- lmer(rob ~ wave + sex + age + educ + white +
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
re_compare4 <- merge(re_A2_4[, c("cntry","re_A2")],
                     re_B2_4[, c("cntry","re_B2")], by = "cntry")
re_compare4$change <- re_compare4$re_B2 - re_compare4$re_A2
it_re4 <- re_compare4[re_compare4$cntry == "IT", ]
cat(sprintf("\n*** ITALIA (wave 1-4) ***\n"))
cat(sprintf("  Residuo Modello A2: %+.3f\n", it_re4$re_A2))
cat(sprintf("  Residuo Modello B2: %+.3f\n", it_re4$re_B2))
cat(sprintf("  Variazione:         %+.3f\n", it_re4$change))
if (it_re4$re_A2 != 0) {
  cat(sprintf("  UAI spiega %.1f%% del residuo italiano\n",
              abs(it_re4$change / it_re4$re_A2) * 100))
}

rm(m_A2, m_B2, m_A2_4, m_B2_4)




#' ===================================================================
#' # 7. Feel items — modelli A2 (replica G&A, wave 1-3)
#' ===================================================================
#' Per brevita' si replicano i modelli A2 sui feel items (G&A Table 4).
#' I feel items non sono disponibili in wave 4 con scala comparabile.

cat("\n\n=== MODELLI A2 FEEL ITEMS (wave 1-3, replica G&A) ===\n")

cat("\n--- feel1: operazione medica ---\n")
lmer.imp(feel1 ~ wave + sex + age + educ + white +
           AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
           (1 | cid),
         data    = dati,
         weights = "wgt2",
         stdy    = TRUE,
         stdx    = FALSE,
         subset  = (dati[[1]]$wave %in% c(1, 2, 3)),
         control = lmerControl(optimizer = "nloptwrap"))

cat("\n--- feel2: robot al lavoro ---\n")
lmer.imp(feel2 ~ wave + sex + age + educ + white +
           AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
           (1 | cid),
         data    = dati,
         weights = "wgt2",
         stdy    = TRUE,
         stdx    = FALSE,
         subset  = (dati[[1]]$wave %in% c(2, 3)),  # feel2 assente in wave 1
         control = lmerControl(optimizer = "nloptwrap"))

cat("\n--- feel3: assistenza anziani ---\n")
lmer.imp(feel3 ~ wave + sex + age + educ + white +
           AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
           (1 | cid),
         data    = dati,
         weights = "wgt2",
         stdy    = TRUE,
         stdx    = FALSE,
         subset  = (dati[[1]]$wave %in% c(2, 3)),
         control = lmerControl(optimizer = "nloptwrap"))

cat("\n--- feel4: auto a guida autonoma ---\n")
lmer.imp(feel4 ~ wave + sex + age + educ + white +
           AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
           (1 | cid),
         data    = dati,
         weights = "wgt2",
         stdy    = TRUE,
         stdx    = FALSE,
         subset  = (dati[[1]]$wave %in% c(2, 3)),
         control = lmerControl(optimizer = "nloptwrap"))




#' ===================================================================
#' # 8. Focus Italia — regressione wave 4 [ESTENSIONE]
#' ===================================================================
#' Analisi sul solo sotto-campione italiano (N~1037, wave 4).
#' Modello di regressione progressivo a blocchi:
#'   Blocco 1 — sociodemografico (sex, age, educ)
#'   Blocco 2 — occupazionale (white)
#'   Blocco 3 — interazione genere x occupazione
#' Non si usa lmer (singolo paese = nessun livello 2).
#' Listwise deletion sul sotto-campione imputato (primo dataset).

cat("\n\n=== FOCUS ITALIA — REGRESSIONE WAVE 4 ===\n")
cat("[ESTENSIONE] Sotto-campione italiano, wave 4 (N~1037)\n\n")
cat("NOTA: lmer non applicabile su un solo paese (livello 2 = 1 unita').\n")
cat("Si usa lm ponderato, pooling dei risultati across 20 imputazioni.\n\n")

#' Helper: pooling lm su lista di dataset imputati
pool_lm <- function(dati_list, formula_str, subset_expr, weight_var = "wgt2") {
  results <- lapply(dati_list, function(x) {
    sub <- x[eval(parse(text = subset_expr), envir = x), ]
    sub <- sub[complete.cases(sub[, all.vars(as.formula(formula_str))]), ]
    fit <- lm(as.formula(formula_str), data = sub,
              weights = sub[[weight_var]])
    list(coef = coef(fit), vcov = diag(vcov(fit)), n = nrow(sub))
  })
  qhat <- sapply(results, `[[`, "coef")
  uhat <- sapply(results, `[[`, "vcov")
  n    <- mean(sapply(results, `[[`, "n"))
  cat(sprintf("N medio per dataset imputato: %.0f\n\n", n))
  testEstimates(qhat = qhat, uhat = uhat)
}

#' -------------------------------------------------------------------
#' ## 8a. Blocco 1: sociodemografico
#' -------------------------------------------------------------------
cat("--- Blocco 1: Sociodemografico (sex, age, educ) ---\n")
pool_lm(dati,
        "rob ~ sex + age + educ",
        "wave == 4 & cntry == 'IT'")

#' -------------------------------------------------------------------
#' ## 8b. Blocco 2: + occupazione
#' -------------------------------------------------------------------
cat("\n--- Blocco 2: + Occupazione (white) ---\n")
pool_lm(dati,
        "rob ~ sex + age + educ + white",
        "wave == 4 & cntry == 'IT'")

#' -------------------------------------------------------------------
#' ## 8c. Confronto EU vs Italia: effetti individuali wave 4
#' -------------------------------------------------------------------
cat("\n--- Confronto effetti individuali: EU vs Italia (wave 4) ---\n")
cat("(Coefficienti standardizzati rispetto a Y)\n")

cat("\nEU wave 4:\n")
lmer.imp(rob ~ sex + age + educ + white + (1 | cid),
         data    = dati,
         weights = "wgt2",
         stdy    = TRUE,
         stdx    = FALSE,
         subset  = (dati[[1]]$wave == 4))

cat("\nItalia wave 4 (lm pooled, non standardizzato):\n")
pool_lm(dati,
        "rob ~ sex + age + educ + white",
        "wave == 4 & cntry == 'IT'")


cat("\n✓ Script 4 completato. Procedere con 5__Plots_extended.R\n")