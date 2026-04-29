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
#' Renders all analytical scripts to HTML and prints session information
#' for reproducibility. Follows G&A's 6.2 Document results.R, adapted
#' to the extended pipeline (4 waves, 2012-2024).
#'
#' NOTE: Each render() call executes the script in a fresh environment.
#' Total runtime may exceed 60 minutes if MICE is re-run from scratch.
#' To avoid re-running MICE, ensure ./data/dat.Rdata is already present
#' before executing this script.


#' **Clear workspace**
rm(list = ls())

#' **Pandoc path** — required when running outside RStudio (e.g., Rscript/terminal).
#' rmarkdown checks RSTUDIO_PANDOC as a fallback if pandoc is not in PATH.
if (nchar(Sys.which("pandoc")) == 0) {
  pandoc_rstudio <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"
  if (file.exists(file.path(pandoc_rstudio, "pandoc")))
    Sys.setenv(RSTUDIO_PANDOC = pandoc_rstudio)
  rm(pandoc_rstudio)
}

#' **Load packages**
library(rmarkdown)

dir.create("./results", showWarnings = FALSE)
proj_root <- getwd()




#' ===================================================================
#' # 1. Render all analytical scripts to HTML
#' ===================================================================

scripts <- list(
  list(input  = "./syntax/1_Load_data_extended.R",
       output = "1_Load_data_extended.html",
       label  = "1. Load data"),
  list(input  = "./syntax/2_1_Descriptives_extended.R",
       output = "2_1_Descriptives_extended.html",
       label  = "2.1 Descriptives"),
  list(input  = "./syntax/2_2_Measurement_invariance_extended.R",
       output = "2_2_Measurement_invariance_extended.html",
       label  = "2.2 Measurement invariance"),
  list(input  = "./syntax/3_Current_and_changes_extended.R",
       output = "3_Current_and_changes_extended.html",
       label  = "3. Current attitudes and changes"),
  list(input  = "./syntax/4_Predictors_extended.R",
       output = "4_Predictors_extended.html",
       label  = "4. Predictors of attitudes"),
  list(input  = "./syntax/5_Plots_extended.R",
       output = "5_Plots_extended.html",
       label  = "5. Plots")
)

for (s in scripts) {
  cat(sprintf("Rendering: %s ...\n", s$label))
  tryCatch(
    render(input         = s$input,
           output_dir    = file.path(proj_root, "results"),
           output_file   = s$output,
           knit_root_dir = proj_root,
           envir         = new.env(parent = globalenv()),
           quiet         = TRUE),
    error = function(e) cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  )
  cat(sprintf("  -> results/%s\n", s$output))
}

cat("\nAll scripts rendered.\n")
rm(scripts, s, proj_root)




#' ===================================================================
#' # 2. R version and package documentation
#' ===================================================================
#' Extracts all library() calls from the analytical scripts and prints
#' sessionInfo() for reproducibility (following G&A's 6.1).

extract_libs <- function(filepath) {
  if (!file.exists(filepath)) return(character(0))
  lines <- readLines(filepath, warn = FALSE)
  libs  <- character(0)
  for (line in lines) {
    g <- gregexpr("library\\((.+?)\\)", line, perl = TRUE)
    if (g[[1]][1] != -1) {
      starts  <- attr(g[[1]], "capture.start")
      lengths <- attr(g[[1]], "capture.length")
      for (i in seq_along(starts))
        libs <- c(libs, trimws(substr(line, starts[i],
                                      starts[i] + lengths[i] - 1)))
    }
  }
  unique(libs[nchar(libs) > 0])
}

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
cat("\nPackages used across all scripts:\n")
print(all_libs)

for (lib in all_libs) {
  suppressPackageStartupMessages(
    tryCatch(library(lib, character.only = TRUE),
             error = function(e) cat(sprintf("  Not available: %s\n", lib)))
  )
}

cat("\n")
print(sessionInfo())
rm(extract_libs, all_scripts, all_libs, lib)
