#' ---
#' title: Document analyses — Extended (4 waves, 2012-2024)
#' author: Camilla Bonomo [extends Gnambs & Appel 2019]
#' output:
#'    html_document:
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---
#'
#' This script serves two purposes:
#'   1. Renders all analytical scripts to HTML for documentation
#'   2. Produces a concise summary of key results for internal use (e.g., thesis writing)
#'
#' Structure:
#'   Section 1 — Render all analytical scripts to HTML
#'   Section 2 — R version and package documentation
#'   Section 3 — Summary of main results (for internal use / supervisor reference)


#' **Clear workspace**
rm(list = ls())

#' **Load packages**
library(rmarkdown)

dir.create("./results", showWarnings = FALSE)




#' ===================================================================
#' # 1. Render all analytical scripts to HTML
#' ===================================================================
#' G&A used render() to produce HTML reports from R scripts annotated with #'.
#' The same approach is applied here, adapted to the extended file names.
#'
#' NOTE: Each render() call executes the script in a clean environment.
#' Total runtime may be 30-60 minutes if MICE is re-executed.
#' To avoid re-running MICE, ensure dat.Rdata is already present in ./data/
#' before executing this script.

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
    label  = "3. Current attitudes and changes (extended)"
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

cat("=== RENDERING ANALYTICAL SCRIPTS TO HTML ===\n\n")
#' knit_root_dir forces the working directory to the project root,
#' preventing knitr from relocating the working directory to ./syntax/.
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
      cat(sprintf("  ERROR in %s: %s\n", lbl, conditionMessage(e)))
    }
  )
  cat(sprintf("  Saved: results/%s\n", out))
  rm(lbl, inp, out)
}
cat("\nRendering complete.\n")




#' ===================================================================
#' # 2. R version and package documentation
#' ===================================================================
#' Package version documentation for reproducibility (following G&A).
#' Extended to include all additional packages used in the extension.

cat("\n=== R VERSION AND PACKAGE DOCUMENTATION ===\n\n")

#' **Extract library() calls from a script file**
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

#' **Collect packages from all analytical scripts**
all_scripts <- c(
  "./syntax/0_Start.R",
  "./syntax/1_Load_data_extended.R",
  "./syntax/2_1_Descriptives_extended.R",
  "./syntax/2_2_Measurement_invariance_extended.R",
  "./syntax/3_Current_and_changes_extended.R",
  "./syntax/4_Predictors_extended.R",
  "./syntax/5_Plots_extended.R"
)

all_libs <- sort(unique(unlist(lapply(all_scripts, extract_libs))))
cat("Packages used across all scripts:\n")
print(all_libs)

#' **Load all packages and print session information**
for (lib in all_libs) {
  suppressPackageStartupMessages(
    tryCatch(library(lib, character.only = TRUE),
             error = function(e) cat(sprintf("  Not available: %s\n", lib)))
  )
}

cat("\n")
print(sessionInfo())




#' ===================================================================
#' # 3. Summary of main results
#' ===================================================================
#' Numerical summary of key findings — for internal use and as a
#' quick reference during thesis writing (Chapter 5).

cat("\n\n=== SUMMARY OF MAIN RESULTS ===\n")
cat("(To be integrated into the thesis text — Chapter 5)\n\n")

#' **Load data**
load("./data/dat.Rdata")
rm(dati_mice)

#' -------------------------------------------------------------------
#' ## 3.1 Longitudinal trend EU27
#' -------------------------------------------------------------------
#' NOTE: Waves 1-3 use the three-item composite (rob; range 0-9).
#' Wave 4 uses the two-item composite (rob2item; range 0-6) because
#' rob3 is not comparable across waves due to a wording change.
#' Means are therefore not directly comparable across the full time series.

cat("--- 3.1 Weighted composite score EU27 by wave ---\n")
cat("    Waves 1-3: three-item composite rob (range 0-9)\n")
cat("    Wave 4:    two-item composite rob2item (range 0-6)\n\n")

anni <- c(2012, 2014, 2017, 2024)

for (w in 1:3) {
  sub <- dat[dat$wave == w & !is.na(dat$rob) & !is.na(dat$wgt2), ]
  m   <- weighted.mean(sub$rob, sub$wgt2)
  n   <- nrow(sub)
  cat(sprintf("  Wave %d (%d): M(rob)     = %.3f  N = %d\n", w, anni[w], m, n))
}

sub4 <- dat[dat$wave == 4 & !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
m4   <- weighted.mean(sub4$rob2item, sub4$wgt2)
n4   <- nrow(sub4)
cat(sprintf("  Wave 4 (2024): M(rob2item) = %.3f  N = %d\n", m4, n4))


#' -------------------------------------------------------------------
#' ## 3.2 Italy vs EU27
#' -------------------------------------------------------------------
cat("\n--- 3.2 Italy vs EU27 ---\n")
cat("    Waves 1-3: rob; Wave 4: rob2item\n\n")

for (w in 1:3) {
  sub_it <- dat[dat$wave == w & dat$cntry == "IT" &
                  !is.na(dat$rob) & !is.na(dat$wgt2), ]
  sub_eu <- dat[dat$wave == w & !is.na(dat$rob) & !is.na(dat$wgt2), ]
  m_it <- weighted.mean(sub_it$rob, sub_it$wgt2)
  m_eu <- weighted.mean(sub_eu$rob, sub_eu$wgt2)
  cat(sprintf("  Wave %d (%d): IT = %.3f  EU = %.3f  diff = %+.3f\n",
              w, anni[w], m_it, m_eu, m_it - m_eu))
}

sub_it4 <- dat[dat$wave == 4 & dat$cntry == "IT" &
                 !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
sub_eu4 <- dat[dat$wave == 4 & !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
m_it4 <- weighted.mean(sub_it4$rob2item, sub_it4$wgt2)
m_eu4 <- weighted.mean(sub_eu4$rob2item, sub_eu4$wgt2)
cat(sprintf("  Wave 4 (2024): IT = %.3f  EU = %.3f  diff = %+.3f\n",
            m_it4, m_eu4, m_it4 - m_eu4))


#' -------------------------------------------------------------------
#' ## 3.3 ICC by wave
#' -------------------------------------------------------------------
cat("\n--- 3.3 Country-level ICC by wave (from Script 4, Model A0) ---\n")
cat("    Waves 1-3: rob; Wave 4: rob2item\n\n")
cat("  Wave 1 (2012): ICC = 0.088\n")
cat("  Wave 2 (2014): ICC = 0.074\n")
cat("  Wave 3 (2017): ICC = 0.057\n")
cat("  Wave 4 (2024): ICC = 0.044\n")
cat("  Trend: cross-national convergence over time (-50% in 12 years)\n")


#' -------------------------------------------------------------------
#' ## 3.4 Key coefficients: Model A2 (waves 1-3, G&A replication)
#' -------------------------------------------------------------------
cat("\n--- 3.4 Significant predictors, Model A2 (waves 1-3) ---\n")
cat("  Individual-level:\n")
cat("    wave2:  b = -0.282***  (increasing scepticism 2012 -> 2014)\n")
cat("    wave3:  b = -0.501***  (increasing scepticism 2012 -> 2017)\n")
cat("    sex1:   b = -0.390***  (women more sceptical; std = -0.203)\n")
cat("    educ:   b = +0.125***  (education as protective factor; std = +0.065)\n")
cat("    white2: b = -0.164***  (blue-collar workers more sceptical)\n")
cat("  Country-level (L2):\n")
cat("    AGEOLD:  b = +0.075***  d = +0.18\n")
cat("    TECHEXP: b = +0.022***  d = +0.16\n")
cat("    LAT:     b = +0.045***  d = +0.32  (North-South divide)\n")
cat("    LONG:    b =  0.002 ns\n")
cat("    INVEST:  b = -0.114 ns (p = .095)\n")
cat("    UNEMP:   b = +0.013*\n")


#' -------------------------------------------------------------------
#' ## 3.5 Model B2: UAI effect and latitude mediation
#' -------------------------------------------------------------------
cat("\n--- 3.5 Model B2: UAI and latitude mediation ---\n")
cat("  H1 (UAI < 0): UAI_z b = -0.180 (p = .154) — correct sign,\n")
cat("     not significant with 27 L2 units (limited statistical power)\n")
cat("  H2 (mediation of LAT by UAI):\n")
cat("     LAT in A2:  b = +0.045***  (p = .002)\n")
cat("     LAT in B2:  b = +0.029 ns  (p = .110) — CONFIRMED\n")
cat("     The North-South gradient is partially explained by UAI\n")
cat("  H3a (UAI x educ): b = +0.027***  (p < .001) — CONFIRMED\n")
cat("     Education compensates more for cultural uncertainty anxiety\n")
cat("     in high-UAI countries\n")
cat("  H3b (UAI x blue-collar): b = -0.025 ns — not confirmed\n")


#' -------------------------------------------------------------------
#' ## 3.6 Italy's residuals in multilevel models
#' -------------------------------------------------------------------
#' NOTE: Waves 1-3 use rob; waves 1-4 use rob2item.
#' Residuals across model specifications are not directly comparable
#' in magnitude because the outcome scales differ (range 0-9 vs 0-6).

cat("\n--- 3.6 Italy's residuals in multilevel models ---\n")
cat("  Robustness check across 20 imputations:\n")
cat("  Model A2, waves 1-3 (rob):      about -0.26\n")
cat("    (Italy reliably below structural expectations)\n")
cat("  Model B2, waves 1-3 (rob):      about -0.37\n")
cat("    (residual becomes more negative with UAI — not mediated by cultural uncertainty)\n")
cat("  Model A2, waves 1-4 (rob2item): about +0.10\n")
cat("  Model B2, waves 1-4 (rob2item): about +0.05\n")
cat("  Wave 4-only model with individual predictors: about -0.09\n")
cat("  CONCLUSION: Italy's negative residual is robust for waves 1-3,\n")
cat("  but it does not generalise to the full 1-4 contextual models.\n")
cat("  The Italian case is therefore specification-sensitive rather than\n")
cat("  uniformly more sceptical than expected.\n")
cat("  Chapter 6 explores Italy-specific mechanisms:\n")
cat("  collective memory of one-company towns, SME-dominated industrial\n")
cat("  structure, industrial relations, and the Piedmontese institutional\n")
cat("  ecosystem.\n")


#' -------------------------------------------------------------------
#' ## 3.7 Italy focus, wave 4
#' -------------------------------------------------------------------
cat("\n--- 3.7 Italy focus, wave 4: predictors vs EU27 ---\n")
cat("  Gender (IT, wave 4):    b = +0.033 ns  — gender gap ABSENT\n")
cat("                           vs EU: b = -0.326***\n")
cat("  Blue-collar (IT):       b = -0.240 ns  — does not replicate EU pattern\n")
cat("                           vs EU: b = -0.364***\n")
cat("  Age (IT):               b = -0.264***  — the only robust predictor\n")
cat("  Education (IT):         b = +0.062*\n")
cat("  CONCLUSION: Standard EU predictors do not operate in the same way\n")
cat("  in Italy in 2024 — requires qualitative investigation.\n")


#' -------------------------------------------------------------------
#' ## 3.8 UAI x composite score correlation
#' -------------------------------------------------------------------
cat("\n--- 3.8 Correlation: UAI x weighted country mean ---\n")
cat("  r(UAI, score_2017) = -0.561\n")
cat("  r(UAI, score_2024) = -0.682\n")
cat("  The correlation STRENGTHENS in 2024 — UAI becomes increasingly\n")
cat("  predictive as public debate about AI grows more salient.\n")


cat("\n\n=== DOCUMENTATION COMPLETE ===\n")
cat("HTML files saved to ./results/\n")
cat("Result summary printed above.\n")
cat("\nAnalytical pipeline complete.\n")
cat("  Next steps:\n")
cat("  - Qualitative data collection (interviews, Chapter 6)\n")
cat("  - Write Chapters 5-7 of the thesis\n")
cat("  - Update methodological appendix\n")
