#' ---
#' title: Figures — Extended (4 waves, 2012-2024)
#' author: Camilla Bonomo [extends Gnambs & Appel 2019]
#' output:
#'    html_document:
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---
#'
#' Figures produced:
#'   Figure 1a — EU map: mean composite score by country (wave 3; G&A replication)
#'   Figure 1b — EU map: attitude change wave 1 -> 3 (G&A replication)
#'   Figure 1c — EU map: mean composite score by country (wave 4; EXTENSION)
#'   Figure 2  — Bar chart: mean attitude by wave and application domain (waves 1-3)
#'   Figure 3a — Quantile regression: individual predictors, waves 1-3 (G&A replication)
#'   Figure 3b — Quantile regression: individual predictors, waves 1-4 (EXTENSION, rob2item)
#'   Figure 4  — Longitudinal trajectory: Italy vs. EU benchmark (rob2item)
#'   Figure 5  — Scatter plot: UAI vs. composite score by country (rob2item)
#'   Figure 6  — Country random effects with Italy highlighted (rob2item)
#'   Figure 7  — ICC decline across waves: convergence of national attitudes (rob2item)
#'   Figure 8  — Gender gap by country (wave 4): dot plot
#'   Figure 9  — Country rank bump chart across all four waves (rob2item)
#'   Figure 10 — Attitude distribution by wave: bar chart (rob2item)
#'   Figure 11 — Italy vs. EU27 by demographic subgroup (wave 4)
#'   Figure 12 — EU map: attitude change wave 3 -> 4 (EXTENSION)
#'   Figure 13 — Age gradient across waves: EU27 and Italy (rob2item)
#'   Figure 14 — Latitude vs. composite score by country (North-South divide)
#'   Figure 15 — Gender gap across waves: EU27 and Italy (rob2item)
#'
#' Dependent variable note:
#'   rob      = three-item composite (rob1+rob2+rob3, range 0-9), waves 1-3 only.
#'   rob2item = two-item comparable composite (rob1+rob2, range 0-6), waves 1-4.
#'   All figures including wave 4 use rob2item. rob is restricted to waves 1-3.
#'
#' Colour scheme:
#'   Palette: Okabe-Ito (2008), colorblind-safe throughout.
#'   Italy highlight  -> #D55E00 (vermilion)
#'   EU27 reference   -> #0072B2 (blue)
#'   Neutral/other    -> #999999 (grey)
#'   Positive/high    -> blue family; Negative/low -> red-orange family


#' **Clear workspace**
rm(list = ls())

#' **Load packages**
library(doBy)
library(ggplot2)
library(ggrepel)
library(mitml)
library(weights)
library(grid)
library(quantreg)
library(dplyr)
library(lme4)
library(lmerTest)

source("./syntax/0_Start.R")

#' **Load data**
load("./data/dat.Rdata")
rm(dati_mice)

#' **Add rob2item and numeric wave to all imputed datasets**
dati <- lapply(dati, function(x) {
  x$rob2item <- x$rob1 + x$rob2
  x$wave_n   <- as.numeric(as.character(x$wave))
  x
})
dati <- as.mitml.list(dati)

# Ensure rob2item is present in raw data as well
dat$rob2item <- dat$rob1 + dat$rob2

dir.create("./plots", showWarnings = FALSE)




#' ===================================================================
#' # GLOBAL COLOUR PALETTE AND THEME
#' ===================================================================
#' Okabe-Ito (2008) colorblind-safe palette used consistently throughout.
#' theme_academic() provides a uniform academic look for all ggplot figures.

# Diverging palette (red-white-blue): used for maps and ICC
pal_div <- colorRampPalette(c("#b2182b", "#ef8a62", "#fddbc7",
                              "#f7f7f7",
                              "#d1e5f0", "#67a9cf", "#2166ac"))

# Primary semantic colours
col_italy   <- "#D55E00"   # Okabe-Ito vermilion (Italy, below EU mean)
col_eu      <- "#0072B2"   # Okabe-Ito blue (EU27 reference)
col_neutral <- "#999999"   # grey (other countries)

# Wave palette: 4 time points, dark-to-warm progression
col_waves <- c("2012" = "#0072B2",
               "2014" = "#56B4E9",
               "2017" = "#E69F00",
               "2024" = "#D55E00")

# Benchmark country colours for Figures 4 and 9 (Okabe-Ito + ColorBrewer Dark2)
col_benchmark <- c(
  IT   = "#D55E00",   # vermilion
  EU27 = "#0072B2",   # blue
  DE   = "#009E73",   # teal
  FR   = "#7570B3",   # purple
  DK   = "#E7298A",   # magenta
  SE   = "#66A61E",   # olive green
  GR   = "#E6AB02",   # gold
  PT   = "#A6761D",   # brown
  ES   = "#666666"    # dark grey
)

#' **Consistent academic theme**
theme_academic <- function(base_size = 12) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      panel.grid.minor    = element_blank(),
      panel.grid.major    = element_line(colour = "grey88", linewidth = 0.3),
      strip.background    = element_rect(fill = "grey94", colour = "grey70",
                                         linewidth = 0.5),
      strip.text          = element_text(size = base_size, face = "bold",
                                         margin = margin(4, 4, 4, 4)),
      legend.background   = element_blank(),
      legend.key          = element_blank(),
      legend.title        = element_text(size = base_size - 1, face = "bold"),
      legend.text         = element_text(size = base_size - 1),
      plot.title          = element_text(size = base_size + 1, face = "bold",
                                         hjust = 0, margin = margin(b = 3)),
      plot.subtitle       = element_text(size = base_size - 1, colour = "grey40",
                                         hjust = 0, margin = margin(b = 5)),
      plot.caption        = element_text(size = base_size - 2, colour = "grey50",
                                         hjust = 0, margin = margin(t = 4)),
      axis.text           = element_text(colour = "grey30", size = base_size - 1),
      axis.title          = element_text(size = base_size, face = "bold"),
      plot.margin         = margin(10, 12, 10, 10)
    )
}




#' ===================================================================
#' # 1. EU maps [G&A replication + EXTENSION wave 4]
#' ===================================================================

maps_available <- requireNamespace("rworldmap", quietly = TRUE)

if (maps_available) {
  library(rworldmap)

  #' **Standardise composite score relative to wave 3** [G&A]
  rob_w3_mean <- mean(sapply(dati, function(x) {
    sub <- x[x$wave_n == 3, ]
    weighted.mean(sub$rob, sub$wgt2, na.rm = TRUE)
  }))
  rob_w3_sd <- mean(sapply(dati, function(x) {
    sub <- x[x$wave_n == 3, ]
    sqrt(wtd.var(sub$rob, sub$wgt2))
  }))

  dati <- lapply(dati, function(x) {
    x$zrob <- (x$rob - rob_w3_mean) / rob_w3_sd
    x
  })
  dati <- as.mitml.list(dati)

  #' **Compute country-level statistics**
  paesi <- unique(dat$cntry)
  ds <- data.frame(cntry = paesi, rob_w3 = NA, rob_w4 = NA,
                   delta_13 = NA, delta_14 = NA, delta_34 = NA,
                   stringsAsFactors = FALSE)

  for (i in paesi) {
    ds$rob_w3[ds$cntry == i] <- mean(sapply(dati, function(x) {
      sub <- x[x$cntry == i & x$wave_n == 3, ]
      if (nrow(sub) > 0) weighted.mean(sub$zrob, sub$wgt2, na.rm = TRUE)
      else NA
    }))

    ds$rob_w4[ds$cntry == i] <- {
      sub <- dat[dat$cntry == i & dat$wave == 4 &
                   !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
      if (nrow(sub) > 0) weighted.mean(sub$rob2item, sub$wgt2) else NA
    }

    if (i != "HR") {
      ds$delta_13[ds$cntry == i] <-
        ttest.imp(rob ~ wave_n, dati, weights = "wgt2",
                  paired = FALSE, print = FALSE,
                  subset = (dati[[1]]$cntry == i &
                              dati[[1]]$wave_n %in% c(1, 3)))$d * -1
    }

    if (i != "HR") {
      ds$delta_14[ds$cntry == i] <-
        ttest.imp(rob2item ~ wave_n, dati, weights = "wgt2",
                  paired = FALSE, print = FALSE,
                  subset = (dati[[1]]$cntry == i &
                              dati[[1]]$wave_n %in% c(1, 4)))$d * -1
    }

    ds$delta_34[ds$cntry == i] <- tryCatch(
      ttest.imp(rob2item ~ wave_n, dati, weights = "wgt2",
                paired = FALSE, print = FALSE,
                subset = (dati[[1]]$cntry == i &
                            dati[[1]]$wave_n %in% c(3, 4)))$d * -1,
      error = function(e) NA
    )
  }

  ds$rob_w3[ds$rob_w3 < -.50] <- -.495

  europeanUnion <- c("Austria", "Belgium", "Bulgaria", "Cyprus",
                     "Czech Rep.", "Denmark", "Estonia", "Finland", "France",
                     "Germany", "Greece", "Croatia", "Hungary", "Ireland",
                     "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
                     "Netherlands", "Poland", "Portugal", "Romania",
                     "Slovakia", "Slovenia", "Spain", "Sweden")

  sPDF <- joinCountryData2Map(ds, joinCode = "ISO2", nameJoinColumn = "cntry")
  sPDFmyCountries <- sPDF[sPDF$NAME %in% europeanUnion, ]

  pal_score    <- pal_div(12)
  pal_change   <- pal_div(12)
  pal_score_w4 <- pal_div(12)

  #' --- Figure 1a: score wave 3 [G&A] ---
  png("./plots/Figure_1a_map_wave3.png",
      width = 7, height = 7, units = "in", res = 150)
  op <- par(fin = c(4, 10), mfcol = c(1, 1),
            mai = c(0.2, 0, 0.2, 0), xaxs = "i", yaxs = "i")
  mapParams <- mapCountryData(
    sPDF,
    nameColumnToPlot = "rob_w3",
    numCats          = 12,
    catMethod        = seq(-0.5, 0.5, length.out = 13),
    xlim             = bbox(sPDFmyCountries)[1, ],
    ylim             = bbox(sPDFmyCountries)[2, ],
    addLegend        = FALSE,
    colourPalette    = pal_score,
    mapTitle         = "Attitudes towards robots, 2017 (standardised)")
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2,
                          legendLabels = "all"))
  par(op); dev.off()
  cat("Saved: Figure_1a_map_wave3.png\n")


  #' --- Figure 1b: change wave 1 -> 3 [G&A] ---
  png("./plots/Figure_1b_map_change13.png",
      width = 7, height = 7, units = "in", res = 150)
  op <- par(fin = c(4, 10), mfcol = c(1, 1),
            mai = c(0.2, 0, 0.2, 0), xaxs = "i", yaxs = "i")
  mapParams <- mapCountryData(
    sPDF,
    nameColumnToPlot = "delta_13",
    numCats          = 12,
    catMethod        = seq(-0.5, 0.3, length.out = 13),
    xlim             = bbox(sPDFmyCountries)[1, ],
    ylim             = bbox(sPDFmyCountries)[2, ],
    addLegend        = FALSE,
    colourPalette    = pal_change,
    mapTitle         = "Attitude change 2012–2017 (Cohen's d, rob)")
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2,
                          legendLabels = "all"))
  par(op); dev.off()
  cat("Saved: Figure_1b_map_change13.png\n")


  #' --- Figure 1c: score wave 4 (rob2item) [EXTENSION] ---
  png("./plots/Figure_1c_map_wave4.png",
      width = 7, height = 7, units = "in", res = 150)
  op <- par(fin = c(4, 10), mfcol = c(1, 1),
            mai = c(0.2, 0, 0.2, 0), xaxs = "i", yaxs = "i")
  mapParams <- mapCountryData(
    sPDF,
    nameColumnToPlot = "rob_w4",
    numCats          = 12,
    catMethod        = seq(2.0, 4.5, length.out = 13),
    xlim             = bbox(sPDFmyCountries)[1, ],
    ylim             = bbox(sPDFmyCountries)[2, ],
    addLegend        = FALSE,
    colourPalette    = pal_score_w4,
    mapTitle         = "Attitudes towards robots and AI, 2024 (rob2item, 0–6)")
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2,
                          legendLabels = "all"))
  par(op); dev.off()
  cat("Saved: Figure_1c_map_wave4.png\n")


  #' --- Figure 12: attitude change 2017 -> 2024 (rob2item) [EXTENSION] ---
  pal_change_34 <- pal_div(12)
  png("./plots/Figure_12_map_change34.png",
      width = 7, height = 7, units = "in", res = 150)
  op <- par(fin = c(4, 10), mfcol = c(1, 1),
            mai = c(0.2, 0, 0.2, 0), xaxs = "i", yaxs = "i")
  mapParams <- mapCountryData(
    sPDF,
    nameColumnToPlot = "delta_34",
    numCats          = 12,
    catMethod        = seq(-0.5, 0.5, length.out = 13),
    xlim             = bbox(sPDFmyCountries)[1, ],
    ylim             = bbox(sPDFmyCountries)[2, ],
    addLegend        = FALSE,
    colourPalette    = pal_change_34,
    mapTitle         = "Attitude change 2017–2024 (Cohen’s d, rob2item)")
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2,
                          legendLabels = "all"))
  par(op); dev.off()
  cat("Saved: Figure_12_map_change34.png\n")

  rm(rob_w3_mean, rob_w3_sd, ds, mapParams, op, sPDF,
     sPDFmyCountries, europeanUnion, paesi, i,
     pal_score, pal_change, pal_score_w4, pal_change_34)

} else {
  cat("rworldmap not available — Figures 1a/1b/1c/12 skipped.\n")
}




#' ===================================================================
#' # 2. Bar chart — mean attitudes by wave [G&A, waves 1-3 only]
#' ===================================================================
#' Wave 4 excluded: rob3 wording changed in 2024 (not comparable).
#' feel1-feel4 are single-item measures on the same 0-9 scale as rob.
#' Error bars show ±½ SD (between-country dispersion).

cat("\n=== FIGURE 2: MEAN ATTITUDES BY WAVE (1-3) ===\n")

get_wave_means <- function(wave_num, items, dati) {
  if (!"wave_n" %in% names(dati[[1]])) {
    dati <- lapply(dati, function(x) {
      x$wave_n <- as.numeric(as.character(x$wave)); x
    })
    dati <- as.mitml.list(dati)
  }
  describe.imp(dati, items = items, weights = "wgt2",
               stats = c("mean", "sd"), digits = 10,
               subset = (dati[[1]]$wave_n == wave_num))
}

w1 <- get_wave_means(1, c("rob", "feel1", "feel2"), dati)
w2 <- get_wave_means(2, c("rob", "feel1", "feel2", "feel3", "feel4"), dati)
w3 <- get_wave_means(3, c("rob", "feel1", "feel2", "feel3", "feel4"), dati)

plot_data <- data.frame(
  wave = rep(c("2012", "2014", "2017"), each = 5),
  item = rep(c("General\nappraisal", "Medical\noperation",
               "Assisting\nat work", "Services\nfor elderly",
               "Driverless\ncars"), 3),
  mean = c(
    w1["rob",   "mean"], w1["feel1", "mean"], w1["feel2", "mean"], NA, NA,
    w2["rob",   "mean"], w2["feel1", "mean"], w2["feel2", "mean"],
    w2["feel3", "mean"], w2["feel4", "mean"],
    w3["rob",   "mean"], w3["feel1", "mean"], w3["feel2", "mean"],
    w3["feel3", "mean"], w3["feel4", "mean"]
  ),
  sd = c(
    w1["rob",   "sd"], w1["feel1", "sd"], w1["feel2", "sd"], NA, NA,
    w2["rob",   "sd"], w2["feel1", "sd"], w2["feel2", "sd"],
    w2["feel3", "sd"], w2["feel4", "sd"],
    w3["rob",   "sd"], w3["feel1", "sd"], w3["feel2", "sd"],
    w3["feel3", "sd"], w3["feel4", "sd"]
  )
)
plot_data$wave <- factor(plot_data$wave, levels = c("2012", "2014", "2017"))
plot_data$item <- factor(plot_data$item,
                         levels = c("General\nappraisal", "Medical\noperation",
                                    "Assisting\nat work", "Services\nfor elderly",
                                    "Driverless\ncars"))

p2 <- ggplot(plot_data[!is.na(plot_data$mean), ],
             aes(x = item, y = mean, fill = wave)) +
  geom_bar(stat = "identity", position = position_dodge(0.82),
           width = 0.75, colour = "white", linewidth = 0.2) +
  geom_errorbar(aes(ymin = mean - sd / 2, ymax = mean + sd / 2),
                position = position_dodge(0.82), width = 0.25,
                colour = "grey30", linewidth = 0.5) +
  scale_fill_manual(values = c("2012" = "#0072B2",
                               "2014" = "#56B4E9",
                               "2017" = "#E69F00"),
                    name = "Survey year") +
  scale_y_continuous(limits = c(0, 9), breaks = 0:9,
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Attitudes towards robots in Europe, 2012–2017",
       subtitle = "Weighted means, EU27. Wave 2024 excluded (rob3 not comparable; see Figure 4).",
       x = NULL,
       y = "Mean attitude score (0–9)",
       caption = "Error bars: ±½ SD. rob = three-item composite (0–9); feel items = single measures (0–9).") +
  theme_academic(base_size = 12) +
  theme(legend.position   = "bottom",
        panel.grid.major.x = element_blank())

ggsave("./plots/Figure_2_attitudes_by_wave.png", p2,
       width = 12, height = 7, dpi = 150)
cat("Saved: Figure_2_attitudes_by_wave.png\n")
rm(w1, w2, w3, plot_data, p2)




#' ===================================================================
#' # 3. Quantile regression [G&A replication + EXTENSION]
#' ===================================================================
#' Figure 3a: waves 1-3, DV = rob (G&A replication).
#' Figure 3b: waves 1-4, DV = rob2item (EXTENSION; two-item comparable composite).
#'
#' Following G&A, estimates are pooled across all m=20 imputed datasets
#' using Rubin's rules (Rubin, 1987). For each tau in seq(0.10, 0.90, 0.05),
#' rq() is fit on every imputed dataset; the pooled estimate is the mean
#' across datasets, and the SE combines within- and between-imputation
#' variance: T = U_bar + (1 + 1/m) * B.

cat("\n=== FIGURE 3: QUANTILE REGRESSION (pooled across m=20) ===\n")

tau_seq <- seq(0.10, 0.90, by = 0.05)
m_imp   <- length(dati)

#' **Rubin's-rules pooling helper for rq()**
#' Returns a data frame: tau | term | estimate | lower | upper
pool_qr <- function(imp_list, formula_obj) {
  m <- length(imp_list)
  do.call(rbind, lapply(tau_seq, function(tau) {
    fits <- lapply(imp_list, function(d) {
      fit <- do.call(rq, list(formula = formula_obj, tau = tau,
                              data = d, weights = d$wgt2))
      sm  <- tryCatch(
        summary(fit, se = "nid", covariance = TRUE),
        error = function(e) summary(fit, se = "iid", covariance = TRUE)
      )
      list(coef = coef(fit), vcov = sm$cov)
    })
    Q    <- do.call(rbind, lapply(fits, `[[`, "coef"))
    Qbar <- colMeans(Q)
    Ubar <- Reduce("+", lapply(fits, `[[`, "vcov")) / m
    B    <- cov(Q)
    Tvar <- Ubar + (1 + 1/m) * B
    se   <- sqrt(diag(Tvar))
    data.frame(tau      = tau,
               term     = names(Qbar),
               estimate = as.numeric(Qbar),
               lower    = as.numeric(Qbar) - 1.96 * se,
               upper    = as.numeric(Qbar) + 1.96 * se,
               row.names = NULL)
  }))
}

#' **Predictor label mapping** (applied to both figures by term position)
pred_labs <- c("Male vs. female", "Age (10 yr)", "Education",
               "White- vs. blue-collar", "White-collar vs. non-employed")

#' **QR plot helper**: takes pooled data frame, filters predictors, draws figure
plot_qr <- function(qr_df, title_str, subtitle_str) {
  # Drop intercept and wave dummies; remaining terms are in formula order
  preds <- qr_df[!grepl("^\\(Intercept\\)|^wave", qr_df$term), ]
  term_order <- unique(preds$term)            # preserves formula order
  preds$label <- factor(pred_labs[match(preds$term, term_order)],
                        levels = pred_labs)
  ggplot(preds, aes(x = tau, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55",
               linewidth = 0.5) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.15, fill = col_eu) +
    geom_line(colour = col_eu, linewidth = 0.7) +
    facet_wrap(~ label, nrow = 2, scales = "fixed") +
    scale_x_continuous(breaks = c(0.25, 0.50, 0.75),
                       labels = c("25th", "50th", "75th"),
                       minor_breaks = NULL) +
    labs(title    = title_str,
         subtitle = subtitle_str,
         x        = "Attitude percentile",
         y        = "Standardised regression weight") +
    theme_academic()
}

#' --- Figure 3a: waves 1-3 (G&A replication, rob, pooled across m=20) ---
#' rob standardised within each imputed dataset using waves 1-3 only.
cat("  Fitting Figure 3a (waves 1-3, rob) across m=20 datasets...\n")

imp_123 <- lapply(dati, function(x) {
  d        <- droplevels(x[x$wave_n %in% 1:3, ])
  d$white  <- as.factor(d$white)
  d$sex    <- as.factor(d$sex)
  d$wave   <- droplevels(as.factor(d$wave))
  d$age_s  <- as.numeric(d$age) / 10
  d$zrob   <- as.numeric(scale(d$rob))
  d
})

qr_3a <- pool_qr(imp_123, zrob ~ wave + sex + age_s + educ + white)

p3a <- plot_qr(
  qr_3a,
  title_str    = "Figure 3a: Quantile regression — individual predictors (waves 1–3, rob)",
  subtitle_str = "Pooled across m=20 imputed datasets (Rubin's rules). Shaded band: 95% CI."
)

ggsave("./plots/Figure_3a_quantreg_wave123.png", p3a,
       width = 12, height = 5, dpi = 150)
cat("Saved: Figure_3a_quantreg_wave123.png\n")


#' --- Figure 3b: waves 1-4 (EXTENSION, rob2item, pooled across m=20) ---
#' rob2item standardised within each imputed dataset using all four waves.
cat("  Fitting Figure 3b (waves 1-4, rob2item) across m=20 datasets...\n")

imp_1234 <- lapply(dati, function(x) {
  x$white    <- as.factor(x$white)
  x$sex      <- as.factor(x$sex)
  x$wave     <- as.factor(x$wave)
  x$age_s    <- as.numeric(x$age) / 10
  x$zrob2item <- as.numeric(scale(x$rob2item))
  x
})

qr_3b <- pool_qr(imp_1234, zrob2item ~ wave + sex + age_s + educ + white)

p3b <- plot_qr(
  qr_3b,
  title_str    = "Figure 3b: Quantile regression — individual predictors (waves 1–4, rob2item)",
  subtitle_str = "Pooled across m=20 imputed datasets (Rubin's rules). Shaded band: 95% CI."
)

ggsave("./plots/Figure_3b_quantreg_wave1234.png", p3b,
       width = 12, height = 5, dpi = 150)
cat("Saved: Figure_3b_quantreg_wave1234.png\n")

rm(imp_123, imp_1234, qr_3a, qr_3b, p3a, p3b,
   tau_seq, m_imp, pred_labs, pool_qr, plot_qr)




#' ===================================================================
#' # 4. Longitudinal trajectory — Italy vs. EU benchmark [EXTENSION]
#' ===================================================================
#' rob2item (range 0-6) used throughout all four waves.

cat("\n=== FIGURE 4: LONGITUDINAL TRAJECTORY (rob2item) ===\n")

benchmark_countries <- c("IT", "DE", "FR", "DK", "SE", "GR", "PT", "ES")
anni <- c(2012, 2014, 2017, 2024)

traj_data <- do.call(rbind, lapply(benchmark_countries, function(cc) {
  do.call(rbind, lapply(1:4, function(w) {
    sub <- dat[dat$cntry == cc & dat$wave == w &
                 !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
    if (nrow(sub) == 0) return(NULL)
    data.frame(cntry = cc, anno = anni[w],
               mean = weighted.mean(sub$rob2item, sub$wgt2))
  }))
}))

eu_traj <- do.call(rbind, lapply(1:4, function(w) {
  sub <- dat[dat$wave == w & !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
  data.frame(cntry = "EU27", anno = anni[w],
             mean = weighted.mean(sub$rob2item, sub$wgt2))
}))
traj_data <- rbind(traj_data, eu_traj)

traj_data$is_key   <- traj_data$cntry %in% c("IT", "EU27")
traj_data$cntry    <- factor(traj_data$cntry,
                              levels = c("IT", "EU27", "DE", "FR",
                                         "DK", "SE", "GR", "PT", "ES"))

paese_sizes <- c(IT = 1.5, EU27 = 1.2,
                 DE = 0.7, FR = 0.7, DK = 0.7,
                 SE = 0.7, GR = 0.7, PT = 0.7, ES = 0.7)
paese_types <- c(IT = "solid", EU27 = "solid",
                 DE = "dashed", FR = "dashed", DK = "dotted",
                 SE = "dotted", GR = "longdash", PT = "longdash", ES = "dotdash")

# Labels only at 2024 to avoid clutter
labels_2024 <- subset(traj_data, anno == 2024)

p4 <- ggplot(traj_data, aes(x = anno, y = mean,
                             colour = cntry, linetype = cntry,
                             linewidth = cntry, group = cntry)) +
  geom_line() +
  geom_point(size = 2) +
  geom_text_repel(data      = labels_2024,
                  aes(label = cntry, colour = cntry),
                  nudge_x   = 2, direction = "y",
                  segment.size  = 0.3, segment.color = "grey60",
                  size      = 3.2, fontface = "bold",
                  hjust     = 0, max.overlaps = Inf) +
  scale_colour_manual(values = col_benchmark, name = "Country",
                      guide  = "none") +
  scale_linetype_manual(values = paese_types,  name = "Country",
                        guide  = "none") +
  scale_linewidth_manual(values = paese_sizes, name = "Country",
                         guide  = "none") +
  scale_x_continuous(breaks = c(2012, 2014, 2017, 2024),
                     expand = expansion(mult = c(0.03, 0.16))) +
  scale_y_continuous(breaks = seq(2.0, 5.5, 0.5),
                     limits = c(NA, NA)) +
  labs(title    = "Attitudes toward robots and AI: Italy vs. EU benchmark (2012–2024)",
       subtitle = "Weighted composite score rob2item (0–6); labels at 2024. IT and EU27 in bold lines.",
       x        = "Survey year",
       y        = "Mean composite score (rob2item, 0–6)",
       caption  = "Source: Eurobarometer waves 1–4. Weighted by post-stratification weight.") +
  theme_academic(base_size = 12)

ggsave("./plots/Figure_4_italy_trajectory.png", p4,
       width = 11, height = 6, dpi = 150)
cat("Saved: Figure_4_italy_trajectory.png\n")
rm(traj_data, eu_traj, p4, labels_2024, benchmark_countries,
   paese_sizes, paese_types)




#' ===================================================================
#' # 5. UAI scatter — by country [EXTENSION]
#' ===================================================================
#' rob2item used for both panels (waves 3 and 4).
#' FIX: geom_smooth uses inherit.aes = FALSE to fit ONE line over ALL countries
#' (not grouped by is_italy). r computed and displayed over all countries.
#' Fixed y-axis enables direct comparison of association strength across waves.

cat("\n=== FIGURE 5: UAI vs. COMPOSITE SCORE (rob2item) ===\n")

scatter_data <- do.call(rbind, lapply(unique(dat$cntry), function(cc) {
  uai_val <- dat$UAI[dat$cntry == cc][1]
  m3 <- {
    sub <- dat[dat$cntry == cc & dat$wave == 3 &
                 !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
    if (nrow(sub) > 0) weighted.mean(sub$rob2item, sub$wgt2) else NA
  }
  m4 <- {
    sub <- dat[dat$cntry == cc & dat$wave == 4 &
                 !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
    if (nrow(sub) > 0) weighted.mean(sub$rob2item, sub$wgt2) else NA
  }
  data.frame(cntry = cc, UAI = uai_val, score_w3 = m3, score_w4 = m4)
}))
scatter_data$is_italy <- scatter_data$cntry == "IT"

r_2017 <- cor(scatter_data$UAI, scatter_data$score_w3, use = "complete.obs")
r_2024 <- cor(scatter_data$UAI, scatter_data$score_w4, use = "complete.obs")
cat(sprintf("  r(UAI, rob2item_2017) = %.3f\n", r_2017))
cat(sprintf("  r(UAI, rob2item_2024) = %.3f\n", r_2024))

scatter_long <- rbind(
  data.frame(cntry    = scatter_data$cntry,
             UAI      = scatter_data$UAI,
             score    = scatter_data$score_w3,
             wave     = "2017",
             is_italy = scatter_data$is_italy),
  data.frame(cntry    = scatter_data$cntry,
             UAI      = scatter_data$UAI,
             score    = scatter_data$score_w4,
             wave     = "2024",
             is_italy = scatter_data$is_italy)
)
scatter_long <- scatter_long[!is.na(scatter_long$score) &
                               !is.na(scatter_long$UAI), ]
scatter_long$wave <- factor(scatter_long$wave, levels = c("2017", "2024"))

y_max <- max(scatter_long$score, na.rm = TRUE)
y_min <- min(scatter_long$score, na.rm = TRUE)
uai_max <- max(scatter_long$UAI, na.rm = TRUE)

r_labels <- data.frame(
  wave  = factor(c("2017", "2024"), levels = c("2017", "2024")),
  label = c(sprintf("italic(r) == %.3f", r_2017),
            sprintf("italic(r) == %.3f", r_2024)),
  UAI   = uai_max,
  score = y_max - 0.05 * (y_max - y_min)
)

p5 <- ggplot(scatter_long, aes(x = UAI, y = score)) +
  # Regression line over ALL countries (inherit.aes = FALSE prevents grouping by is_italy)
  geom_smooth(inherit.aes = FALSE,
              mapping      = aes(x = UAI, y = score),
              method       = "lm", se = TRUE,
              colour       = "grey40", fill = "grey80",
              linewidth    = 0.8, linetype = "dashed") +
  geom_point(aes(colour = is_italy, size = is_italy)) +
  geom_text_repel(aes(label = cntry, colour = is_italy),
                  size = 2.8, max.overlaps = 20,
                  segment.size = 0.2, segment.color = "grey60") +
  geom_text(data        = r_labels,
            aes(x = UAI, y = score, label = label),
            inherit.aes = FALSE,
            parse       = TRUE,
            hjust = 1, vjust = 1,
            size = 4, fontface = "italic", colour = "grey25") +
  scale_colour_manual(values = c("FALSE" = col_neutral, "TRUE" = col_italy),
                      labels = c("EU countries", "Italy"), name = NULL) +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4), guide = "none") +
  facet_wrap(~ wave, scales = "fixed",
             labeller = labeller(wave = c("2017" = "2017 (rob2item)",
                                          "2024" = "2024 (rob2item)"))) +
  labs(title    = "Uncertainty Avoidance Index and robot attitudes by EU country",
       subtitle = "Each point = one country; dashed line = OLS fit (95% CI) over all 27 countries.",
       x        = "Hofstede Uncertainty Avoidance Index (UAI)",
       y        = "Weighted mean score (rob2item, 0–6)",
       caption  = "Fixed y-axis enables direct comparison of association strength across waves.") +
  theme_academic(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("./plots/Figure_5_UAI_scatter.png", p5,
       width = 12, height = 6, dpi = 150)
cat("Saved: Figure_5_UAI_scatter.png\n")
rm(scatter_data, scatter_long, r_labels, r_2017, r_2024, p5,
   y_max, y_min, uai_max)




#' ===================================================================
#' # 6. Country random effects with Italy highlighted [EXTENSION]
#' ===================================================================
#' Models A2 and B2 fitted on rob2item (waves 1-4). Random effects (BLUPs)
#' averaged across all m=20 imputed datasets.

cat("\n=== FIGURE 6: COUNTRY RANDOM EFFECTS ===\n")

load("./data/dat.Rdata")
rm(dati_mice)
dati <- lapply(dati, function(x) {
  x$rob2item <- x$rob1 + x$rob2
  x
})
dati <- as.mitml.list(dati)

dati <- within(dati, {
  white <- as.factor(white)
  sex   <- as.factor(sex)
  wave  <- as.factor(wave)
  age   <- scale(age,  scale = FALSE) / 10
  educ  <- scale(educ, scale = FALSE)
})

uai_mean <- mean(unique(dati[[1]][, c("cid", "UAI")])$UAI, na.rm = TRUE)
uai_sd   <- sd(unique(dati[[1]][,  c("cid", "UAI")])$UAI,  na.rm = TRUE)
dati <- lapply(dati, function(x) { x$UAI_z <- (x$UAI - uai_mean) / uai_sd; x })
dati <- as.mitml.list(dati)

m_A2_list <- lapply(dati, function(x) {
  lmer(rob2item ~ wave + sex + age + educ + white +
         AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
         (1 | cid),
       data = x, weights = x$wgt2,
       control = lmerControl(optimizer = "nloptwrap"))
})

m_B2_list <- lapply(dati, function(x) {
  lmer(rob2item ~ wave + sex + age + educ + white +
         AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG + UAI_z +
         (1 | cid),
       data = x, weights = x$wgt2,
       control = lmerControl(optimizer = "nloptwrap"))
})

re_A2 <- Reduce("+", lapply(m_A2_list, function(m) ranef(m)$cid)) /
  length(m_A2_list)
re_B2 <- Reduce("+", lapply(m_B2_list, function(m) ranef(m)$cid)) /
  length(m_B2_list)

re_A2$cid <- as.integer(rownames(re_A2))
re_B2$cid <- as.integer(rownames(re_B2))
cid_cntry <- unique(dati[[1]][, c("cid", "cntry")])
re_A2 <- merge(re_A2, cid_cntry, by = "cid")
re_B2 <- merge(re_B2, cid_cntry, by = "cid")
names(re_A2)[2] <- "re_A2"
names(re_B2)[2] <- "re_B2"

re_long <- rbind(
  data.frame(cntry = re_A2$cntry, re = re_A2$re_A2, model = "A2 (without UAI)"),
  data.frame(cntry = re_B2$cntry, re = re_B2$re_B2, model = "B2 (with UAI)")
)
re_long$is_italy <- re_long$cntry == "IT"

ord <- re_A2$cntry[order(re_A2$re_A2)]
re_long$cntry <- factor(re_long$cntry, levels = ord)

it_pos  <- which(levels(re_long$cntry) == "IT")
re_range <- range(re_long$re, na.rm = TRUE)

p6 <- ggplot(re_long, aes(x = cntry, y = re)) +
  geom_hline(yintercept = 0, colour = "grey40", linetype = "dashed",
             linewidth = 0.5) +
  geom_line(aes(group = cntry, colour = is_italy),
            linewidth = 0.6, alpha = 0.5) +
  geom_point(aes(colour = is_italy, shape = model), size = 2.5) +
  annotate("text", x = it_pos, y = re_range[1] - 0.05,
           label = "Italy", colour = col_italy,
           size = 3.5, fontface = "bold") +
  annotate("text", x = length(levels(re_long$cntry)) - 1,
           y = re_range[2] + 0.03,
           label = "Above prediction", colour = col_eu,
           size = 2.8, fontface = "italic", hjust = 0.5) +
  annotate("text", x = 2, y = re_range[1] + 0.03,
           label = "Below prediction", colour = col_italy,
           size = 2.8, fontface = "italic", hjust = 0.5) +
  scale_colour_manual(values = c("FALSE" = col_neutral, "TRUE" = col_italy),
                      labels = c("Other EU countries", "Italy"), name = NULL) +
  scale_shape_manual(values = c("A2 (without UAI)" = 16, "B2 (with UAI)" = 17),
                     name = "Model") +
  coord_cartesian(clip = "off") +
  labs(title    = "Country random effects: Model A2 vs. B2 (waves 1–4, rob2item)",
       subtitle = "Lines connect A2 → B2 per country. Negative = below structural prediction.",
       x        = "Country (ordered by A2 random intercept)",
       y        = "Random intercept",
       caption  = "Models fitted on rob2item across m = 20 imputed datasets; BLUPs averaged.") +
  theme_academic(base_size = 11) +
  theme(axis.text.x    = element_text(angle = 45, hjust = 1, size = 8.5),
        legend.position = "bottom",
        legend.box      = "horizontal",
        plot.margin     = margin(10, 12, 15, 10))

ggsave("./plots/Figure_6_random_effects.png", p6,
       width = 13, height = 6.5, dpi = 150)
cat("Saved: Figure_6_random_effects.png\n")
rm(m_A2_list, m_B2_list, re_A2, re_B2, re_long, ord, p6,
   uai_mean, uai_sd, cid_cntry, it_pos, re_range)




#' ===================================================================
#' # 7. ICC decline across waves — convergence [EXTENSION]
#' ===================================================================
#' rob2item used for all four waves: same construct across time, so the
#' declining ICC reflects genuine convergence of national attitudes rather
#' than a change in measurement instrument.

cat("\n=== FIGURE 7: ICC DECLINE ACROSS WAVES ===\n")

icc_vals <- do.call(rbind, lapply(1:4, function(w) {
  vars <- sapply(dati, function(x) {
    wave_int <- as.integer(as.character(x$wave))
    sub <- x[!is.na(wave_int) & wave_int == w, ]
    fit <- lmer(rob2item ~ 1 + (1 | cid),
                data = sub, weights = sub$wgt2,
                control = lmerControl(optimizer = "nloptwrap"))
    vc    <- as.data.frame(VarCorr(fit))
    v_cid <- vc$vcov[vc$grp == "cid"]
    v_res <- vc$vcov[vc$grp == "Residual"]
    v_cid / (v_cid + v_res)
  })
  data.frame(wave = c(2012, 2014, 2017, 2024)[w], ICC = mean(vars))
}))

pct_change <- round((icc_vals$ICC[4] / icc_vals$ICC[1] - 1) * 100)
cat(sprintf("  ICC change 2012→2024: %+d%%\n", pct_change))

p7 <- ggplot(icc_vals, aes(x = wave, y = ICC)) +
  geom_col(fill = col_eu, width = 1.8, alpha = 0.75, colour = "white") +
  geom_line(colour = col_italy, linewidth = 1.3) +
  geom_point(colour = col_italy, size = 4.5, shape = 16) +
  geom_text(aes(label = sprintf("%.3f", ICC), y = ICC + 0.004),
            size = 4.2, fontface = "bold", colour = "grey20") +
  scale_x_continuous(breaks = c(2012, 2014, 2017, 2024)) +
  scale_y_continuous(limits = c(0, 0.11),
                     breaks = seq(0, 0.10, 0.02),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title    = "Between-country variance (ICC) across waves",
       subtitle = "rob2item (same construct across all waves): declining ICC indicates convergence of EU attitudes.",
       x        = "Survey year",
       y        = "ICC (% variance attributable to country)",
       caption  = sprintf("Null multilevel model; averaged over m = 20 imputed datasets. ICC change 2012→2024: %+d%%.", pct_change)) +
  theme_academic(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

ggsave("./plots/Figure_7_icc_convergence.png", p7,
       width = 9, height = 6, dpi = 150)
cat("Saved: Figure_7_icc_convergence.png\n")
rm(icc_vals, p7, pct_change)




#' ===================================================================
#' # 8. Gender gap by country — wave 4 [EXTENSION]
#' ===================================================================
#' Gender gap = women's mean minus men's mean rob2item.
#' Negative = women more sceptical. EU mean reference line added.

cat("\n=== FIGURE 8: GENDER GAP BY COUNTRY (wave 4) ===\n")

gap_data <- do.call(rbind, lapply(unique(dat$cntry), function(cc) {
  sub   <- dat[dat$cntry == cc & dat$wave == 4 &
                 !is.na(dat$rob2item) & !is.na(dat$sex) & !is.na(dat$wgt2), ]
  sub_f <- sub[sub$sex == 1, ]
  sub_m <- sub[sub$sex == 0, ]
  if (nrow(sub_f) < 10 || nrow(sub_m) < 10) return(NULL)
  m_f <- weighted.mean(sub_f$rob2item, sub_f$wgt2)
  m_m <- weighted.mean(sub_m$rob2item, sub_m$wgt2)
  data.frame(cntry = cc, gap = m_f - m_m,
             m_female = m_f, m_male = m_m,
             is_italy = cc == "IT")
}))

gap_data  <- gap_data[order(gap_data$gap), ]
gap_data$cntry <- factor(gap_data$cntry, levels = gap_data$cntry)
eu_mean_gap    <- mean(gap_data$gap, na.rm = TRUE)
cat(sprintf("  EU mean gender gap: %.3f\n", eu_mean_gap))

p8 <- ggplot(gap_data, aes(x = gap, y = cntry, colour = is_italy)) +
  geom_vline(xintercept = 0, colour = "grey40",
             linetype = "solid", linewidth = 0.4) +
  geom_vline(xintercept = eu_mean_gap, colour = col_eu,
             linetype = "dotted", linewidth = 0.8, alpha = 0.9) +
  annotate("text",
           x     = eu_mean_gap + 0.015,
           y     = levels(gap_data$cntry)[round(nrow(gap_data) * 0.97)],
           label = sprintf("EU mean\n%.2f", eu_mean_gap),
           colour = col_eu, size = 3, hjust = 0, fontface = "italic") +
  geom_segment(aes(x = 0, xend = gap, y = cntry, yend = cntry,
                   colour = is_italy), linewidth = 0.9) +
  geom_point(aes(size = is_italy)) +
  scale_colour_manual(values = c("FALSE" = col_neutral, "TRUE" = col_italy),
                      guide = "none") +
  scale_size_manual(values = c("FALSE" = 2.2, "TRUE" = 4.5), guide = "none") +
  scale_x_continuous(breaks = seq(-0.8, 0.5, 0.2)) +
  labs(title    = "Gender gap in robot attitudes by country (wave 4, 2024)",
       subtitle = "Gap = women’s mean − men’s mean (rob2item, 0–6). Italy in orange.",
       x        = "Gender gap (female − male)",
       y        = NULL,
       caption  = "Dotted vertical line = EU27 unweighted mean gap. Negative = women more sceptical.") +
  theme_academic(base_size = 11) +
  theme(panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
        axis.text.y        = element_text(size = 9))

ggsave("./plots/Figure_8_gender_gap_by_country.png", p8,
       width = 10, height = 9, dpi = 150)
cat("Saved: Figure_8_gender_gap_by_country.png\n")
rm(gap_data, p8, eu_mean_gap)




#' ===================================================================
#' # 9. Country rank bump chart across waves [EXTENSION]
#' ===================================================================
#' Ranks countries by weighted mean rob2item within each wave.
#' Highlighted countries labelled at 2024 (ggrepel avoids overlap).

cat("\n=== FIGURE 9: COUNTRY RANK EVOLUTION ===\n")

anni <- c(2012, 2014, 2017, 2024)

country_means <- do.call(rbind, lapply(unique(dat$cntry), function(cc) {
  do.call(rbind, lapply(1:4, function(w) {
    sub <- dat[dat$cntry == cc & dat$wave == w &
                 !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
    if (nrow(sub) == 0) return(NULL)
    data.frame(cntry = cc, anno = anni[w],
               mean_rob2 = weighted.mean(sub$rob2item, sub$wgt2))
  }))
}))

country_means <- country_means %>%
  group_by(anno) %>%
  mutate(rank = rank(-mean_rob2, ties.method = "average")) %>%
  ungroup()

highlight <- c("IT", "DE", "DK", "SE", "GR", "FR")
country_means$highlight   <- country_means$cntry %in% highlight
country_means$cntry_label <- ifelse(country_means$highlight,
                                    country_means$cntry, NA_character_)

hl_colors <- c(IT = col_italy, DE = "#009E73", DK = "#E7298A",
               SE = "#66A61E", GR = "#E6AB02", FR = "#7570B3")

p9 <- ggplot(country_means, aes(x = anno, y = rank, group = cntry)) +
  geom_line(data = subset(country_means, !highlight),
            colour = "grey82", linewidth = 0.4, alpha = 0.8) +
  geom_point(data = subset(country_means, !highlight),
             colour = "grey82", size = 1.2) +
  geom_line(data = subset(country_means, highlight),
            aes(colour = cntry), linewidth = 1.3) +
  geom_point(data = subset(country_means, highlight),
             aes(colour = cntry), size = 2.8) +
  geom_text_repel(data      = subset(country_means, highlight & anno == 2024),
                  aes(label = cntry, colour = cntry),
                  nudge_x   = 1.5, direction = "y",
                  segment.size  = 0.3, segment.color = "grey55",
                  size      = 3.5, fontface = "bold",
                  hjust     = 0, max.overlaps = Inf) +
  scale_colour_manual(values = hl_colors, guide = "none") +
  scale_x_continuous(breaks  = c(2012, 2014, 2017, 2024),
                     expand  = expansion(mult = c(0.05, 0.15))) +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 27),
                  labels = c("1st", "5th", "10th", "15th", "20th", "27th")) +
  labs(title    = "Country rankings in robot acceptance across waves (rob2item)",
       subtitle = "Rank 1 = most favourable. Grey lines = all EU27. Highlighted countries labelled at 2024.",
       x        = "Survey year",
       y        = "Country rank (1 = most favourable)",
       caption  = "Croatia (HR) absent in wave 1 (2012).") +
  theme_academic(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave("./plots/Figure_9_country_rank_bumpchart.png", p9,
       width = 11, height = 8, dpi = 150)
cat("Saved: Figure_9_country_rank_bumpchart.png\n")
rm(country_means, p9, highlight, hl_colors)




#' ===================================================================
#' # 10. Attitude distribution by wave — bar chart [EXTENSION]
#' ===================================================================
#' Weighted proportion of respondents at each rob2item value (0-6) by wave.

cat("\n=== FIGURE 10: ATTITUDE DISTRIBUTION BY WAVE ===\n")

dist_data <- dat[!is.na(dat$rob2item) & !is.na(dat$wgt2), ]
dist_data$Year <- factor(c("2012", "2014", "2017", "2024")[dist_data$wave],
                         levels = c("2012", "2014", "2017", "2024"))

prop_tab <- dist_data %>%
  group_by(Year, rob2item) %>%
  summarise(sum_wgt = sum(wgt2), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(prop = sum_wgt / sum(sum_wgt)) %>%
  ungroup()

p10 <- ggplot(prop_tab, aes(x = factor(rob2item), y = prop,
                             fill = Year, colour = Year)) +
  geom_col(position = position_dodge(0.85), alpha = 0.85,
           width = 0.8, colour = "white") +
  scale_fill_manual(values   = col_waves, name = "Survey year") +
  scale_y_continuous(labels  = scales::percent_format(accuracy = 1),
                     expand  = expansion(mult = c(0, 0.04))) +
  scale_x_discrete(labels = c("0\n(very neg.)", "1", "2", "3",
                               "4", "5", "6\n(very pos.)")) +
  labs(title    = "Distribution of robot attitudes across waves (rob2item, 0–6)",
       subtitle = "Weighted proportions by score value, EU27.",
       x        = "Composite score (rob2item)",
       y        = "Proportion of respondents",
       caption  = "Leftward shift over time indicates rising scepticism.") +
  theme_academic(base_size = 12) +
  theme(legend.position    = "bottom",
        panel.grid.major.x = element_blank())

ggsave("./plots/Figure_10_attitude_distribution.png", p10,
       width = 11, height = 6, dpi = 150)
cat("Saved: Figure_10_attitude_distribution.png\n")
rm(dist_data, prop_tab, p10)




#' ===================================================================
#' # 11. Italy vs. EU27 by demographic subgroup — wave 4 [EXTENSION]
#' ===================================================================
#' Three independent dimensions (Gender, Education, Employment) in separate
#' facet panels, each with its own x-axis. Subgroup factor levels are
#' explicitly ordered after rbind to prevent alphabetical resorting.

cat("\n=== FIGURE 11: ITALY VS. EU27 BY SUBGROUP (wave 4) ===\n")

sub4 <- dat[dat$wave == 4 & !is.na(dat$rob2item) &
              !is.na(dat$sex) & !is.na(dat$educ) &
              !is.na(dat$white) & !is.na(dat$wgt2), ]

# Education tertiles via ntile() — avoids duplicate-breaks errors on ordinal scale
sub4$educ_tert <- factor(
  ntile(sub4$educ, 3),
  labels = c("Low\neducation", "Medium\neducation", "High\neducation")
)
sub4$sex_label <- factor(
  ifelse(sub4$sex == 0, "Male", "Female"),
  levels = c("Male", "Female")
)
sub4$empl_label <- factor(
  c("White-collar", "Blue-collar", "Non-employed")[as.integer(sub4$white)],
  levels = c("White-collar", "Blue-collar", "Non-employed")
)
sub4$group <- ifelse(sub4$cntry == "IT", "Italy", "EU27")

sg_sex <- sub4 %>%
  group_by(group, subgroup = sex_label) %>%
  summarise(mean_rob = weighted.mean(rob2item, wgt2, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dimension = "Gender", subgroup = as.character(subgroup))

sg_educ <- sub4 %>%
  filter(!is.na(educ_tert)) %>%
  group_by(group, subgroup = educ_tert) %>%
  summarise(mean_rob = weighted.mean(rob2item, wgt2, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dimension = "Education", subgroup = as.character(subgroup))

sg_empl <- sub4 %>%
  filter(!is.na(empl_label)) %>%
  group_by(group, subgroup = empl_label) %>%
  summarise(mean_rob = weighted.mean(rob2item, wgt2, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dimension = "Employment", subgroup = as.character(subgroup))

sg_all <- rbind(sg_sex, sg_educ, sg_empl)

# Explicit factor level ordering per dimension — prevents alphabetical resorting
# that occurs when rbind converts different factors to character.
sg_all$subgroup <- factor(
  sg_all$subgroup,
  levels = c(
    "Male", "Female",
    "Low\neducation", "Medium\neducation", "High\neducation",
    "White-collar", "Blue-collar", "Non-employed"
  )
)
sg_all$group     <- factor(sg_all$group,     levels = c("EU27", "Italy"))
sg_all$dimension <- factor(sg_all$dimension, levels = c("Gender", "Education", "Employment"))

p11 <- ggplot(sg_all, aes(x = subgroup, y = mean_rob,
                            colour = group, group = group)) +
  geom_line(linewidth = 1.0, alpha = 0.85) +
  geom_point(aes(shape = group), size = 3.5) +
  scale_colour_manual(values = c("EU27" = col_eu, "Italy" = col_italy),
                      name = NULL) +
  scale_shape_manual(values = c("EU27" = 16, "Italy" = 17), name = NULL) +
  facet_wrap(~ dimension, scales = "free_x", nrow = 1) +
  labs(title    = "Italy vs. EU27: robot attitudes by demographic subgroup (wave 4, 2024)",
       subtitle = "Weighted mean rob2item (0–6). Each panel is an independent dimension.",
       x        = NULL,
       y        = "Mean composite score (rob2item, 0–6)",
       caption  = "Education: tertiles via ntile() on the pooled wave-4 sample. Employment: white-collar / blue-collar / non-employed.") +
  theme_academic(base_size = 12) +
  theme(legend.position  = "top",
        axis.text.x      = element_text(size = 9, angle = 25, hjust = 1),
        panel.grid.minor = element_blank())

ggsave("./plots/Figure_11_italy_subgroup_profile.png", p11,
       width = 13, height = 6, dpi = 150)
cat("Saved: Figure_11_italy_subgroup_profile.png\n")
rm(sub4, sg_sex, sg_educ, sg_empl, sg_all, p11)




#' ===================================================================
#' # 13. Age gradient across waves: EU27 and Italy [EXTENSION]
#' ===================================================================
#' Age tertiles defined globally (pooled across all waves and countries)
#' to ensure temporal comparability of the gradient.

cat("\n=== FIGURE 13: AGE GRADIENT ACROSS WAVES ===\n")

age_q <- quantile(dat$age, probs = c(1/3, 2/3), na.rm = TRUE)
cat(sprintf("  Age tertile boundaries: ≤44 | 45–57 | ≥58 (approx.)\n"))

dat$age_tert <- factor(
  ntile(dat$age, 3),
  labels = c("Young", "Middle-aged", "Older")
)

age_data <- do.call(rbind, lapply(c("EU27", "IT"), function(grp) {
  do.call(rbind, lapply(1:4, function(w) {
    sub <- dat[dat$wave == w & !is.na(dat$rob2item) &
                 !is.na(dat$age_tert) & !is.na(dat$wgt2), ]
    if (grp != "EU27") sub <- sub[sub$cntry == grp, ]
    do.call(rbind, lapply(levels(dat$age_tert), function(at) {
      s <- sub[sub$age_tert == at, ]
      if (nrow(s) < 10) return(NULL)
      data.frame(
        group     = grp,
        anno      = anni[w],
        age_group = at,
        mean      = weighted.mean(s$rob2item, s$wgt2, na.rm = TRUE),
        n         = nrow(s)
      )
    }))
  }))
}))

age_data$group     <- factor(age_data$group,
                              levels = c("EU27", "IT"),
                              labels = c("EU27", "Italy"))
age_data$age_group <- factor(age_data$age_group,
                              levels = c("Young", "Middle-aged", "Older"))

# Okabe-Ito colors: blue (young), orange (middle), vermilion (older)
col_age <- c(Young         = "#56B4E9",
             "Middle-aged" = "#E69F00",
             Older         = "#D55E00")

p13 <- ggplot(age_data, aes(x = anno, y = mean,
                              colour   = age_group,
                              linetype = age_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ group, ncol = 2) +
  scale_x_continuous(breaks = c(2012, 2014, 2017, 2024)) +
  scale_colour_manual(values = col_age, name = "Age group") +
  scale_linetype_manual(
    values = c(Young = "solid", "Middle-aged" = "dashed", Older = "dotted"),
    name   = "Age group") +
  labs(title    = "Age gradient in robot attitudes across waves: EU27 and Italy",
       subtitle = "Weighted mean rob2item (0–6) by age tertile (pooled global definition).",
       x        = "Survey year",
       y        = "Mean composite score (rob2item, 0–6)",
       caption  = "Tertiles computed on the pooled EU27 sample across all four waves.") +
  theme_academic(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("./plots/Figure_13_age_gradient.png", p13,
       width = 12, height = 6, dpi = 150)
cat("Saved: Figure_13_age_gradient.png\n")
rm(age_q, age_data, p13, col_age)




#' ===================================================================
#' # 14. Latitude vs. composite score by country [EXTENSION]
#' ===================================================================
#' Two panels (2017, 2024) with fixed y-axis for direct comparison.
#' Regression line fitted over all countries in each panel.

cat("\n=== FIGURE 14: LATITUDE vs. COMPOSITE SCORE ===\n")

lat_data <- do.call(rbind, lapply(unique(dat$cntry), function(cc) {
  lat_val <- dat$LAT[dat$cntry == cc][1]
  do.call(rbind, lapply(c(3, 4), function(w) {
    sub <- dat[dat$cntry == cc & dat$wave == w &
                 !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
    if (nrow(sub) == 0) return(NULL)
    data.frame(
      cntry      = cc,
      wave_label = ifelse(w == 3, "2017", "2024"),
      LAT        = lat_val,
      mean_rob   = weighted.mean(sub$rob2item, sub$wgt2),
      is_italy   = cc == "IT"
    )
  }))
}))
lat_data$wave_label <- factor(lat_data$wave_label, levels = c("2017", "2024"))

r_lat <- tapply(seq_len(nrow(lat_data)), lat_data$wave_label, function(idx) {
  d <- lat_data[idx, ]
  round(cor(d$LAT, d$mean_rob, use = "complete.obs"), 3)
})
cat(sprintf("  r(LAT, rob2item_2017) = %.3f\n", r_lat["2017"]))
cat(sprintf("  r(LAT, rob2item_2024) = %.3f\n", r_lat["2024"]))

r_labels14 <- data.frame(
  wave_label = factor(c("2017", "2024"), levels = c("2017", "2024")),
  label      = c(sprintf("italic(r) == %.3f", r_lat["2017"]),
                 sprintf("italic(r) == %.3f", r_lat["2024"])),
  LAT        = min(lat_data$LAT, na.rm = TRUE) + 0.5,
  mean_rob   = max(lat_data$mean_rob, na.rm = TRUE) * 0.992
)

p14 <- ggplot(lat_data, aes(x = LAT, y = mean_rob)) +
  geom_smooth(inherit.aes = FALSE,
              mapping      = aes(x = LAT, y = mean_rob),
              method       = "lm", se = TRUE,
              colour       = "grey40", fill = "grey80",
              linewidth    = 0.8, linetype = "dashed") +
  geom_point(aes(colour = is_italy, size = is_italy)) +
  geom_text_repel(aes(label = cntry, colour = is_italy),
                  size = 2.8, max.overlaps = 20,
                  segment.size = 0.2, segment.color = "grey60") +
  geom_text(data        = r_labels14,
            aes(x = LAT, y = mean_rob, label = label),
            inherit.aes = FALSE,
            parse       = TRUE,
            hjust = 0, size = 4, fontface = "italic", colour = "grey25") +
  scale_colour_manual(
    values = c("FALSE" = col_neutral, "TRUE" = col_italy),
    labels = c("EU countries", "Italy"), name = NULL) +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4.5), guide = "none") +
  facet_wrap(~ wave_label, ncol = 2,
             labeller = labeller(wave_label = c("2017" = "2017 (rob2item)",
                                                "2024" = "2024 (rob2item)"))) +
  labs(title    = "North-South gradient in robot acceptance (latitude vs. country mean)",
       subtitle = "Weighted mean rob2item (0–6) per country. Dashed line = OLS fit (95% CI) over all countries.",
       x        = "Country latitude (degrees North)",
       y        = "Weighted mean score (rob2item, 0–6)",
       caption  = "Fixed y-axis enables visual comparison of gradient strength across waves.") +
  theme_academic(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("./plots/Figure_14_latitude_gradient.png", p14,
       width = 12, height = 6, dpi = 150)
cat("Saved: Figure_14_latitude_gradient.png\n")
rm(lat_data, r_lat, r_labels14, p14)




#' ===================================================================
#' # 15. Gender gap across waves: EU27 and Italy [EXTENSION]
#' ===================================================================
#' Longitudinal perspective on the wave-4 finding: gender gap absent
#' in Italy (b = +0.033 ns) vs. persistent EU gap (b = -0.326***).
#' CI bands use weighted SD; n is unweighted (conservative approximation).

cat("\n=== FIGURE 15: GENDER GAP ACROSS WAVES ===\n")

compute_gender_means <- function(data_sub, group_label) {
  do.call(rbind, lapply(1:4, function(w) {
    do.call(rbind, lapply(c(0, 1), function(sx) {
      s <- data_sub[data_sub$wave == w &
                      data_sub$sex  == sx &
                      !is.na(data_sub$rob2item) &
                      !is.na(data_sub$wgt2), ]
      if (nrow(s) < 10) return(NULL)
      m   <- weighted.mean(s$rob2item, s$wgt2, na.rm = TRUE)
      wsd <- sqrt(sum(s$wgt2 * (s$rob2item - m)^2, na.rm = TRUE) /
                    sum(s$wgt2, na.rm = TRUE))
      se  <- wsd / sqrt(nrow(s))
      data.frame(
        group = group_label,
        anno  = c(2012, 2014, 2017, 2024)[w],
        sex   = ifelse(sx == 0, "Male", "Female"),
        mean  = m,
        lower = m - 1.96 * se,
        upper = m + 1.96 * se
      )
    }))
  }))
}

gend_eu  <- compute_gender_means(dat, "EU27")
gend_it  <- compute_gender_means(dat[dat$cntry == "IT", ], "Italy")
gend_all <- rbind(gend_eu, gend_it)
gend_all$sex   <- factor(gend_all$sex,   levels = c("Male", "Female"))
gend_all$group <- factor(gend_all$group, levels = c("EU27", "Italy"))

for (grp in c("EU27", "Italy")) {
  cat(sprintf("\n  %s gender gap (Female − Male) by wave:\n", grp))
  sub_g <- gend_all[gend_all$group == grp, ]
  for (yr in c(2012, 2014, 2017, 2024)) {
    mf <- sub_g$mean[sub_g$anno == yr & sub_g$sex == "Female"]
    mm <- sub_g$mean[sub_g$anno == yr & sub_g$sex == "Male"]
    if (length(mf) > 0 && length(mm) > 0)
      cat(sprintf("    %d: gap = %+.3f\n", yr, mf - mm))
  }
}

# Colour: blue = male, vermilion = female (Okabe-Ito)
col_sex <- c(Male = "#0072B2", Female = "#D55E00")

p15 <- ggplot(gend_all,
              aes(x        = anno,
                  y        = mean,
                  colour   = sex,
                  linetype = group,
                  group    = interaction(sex, group))) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex),
              alpha = 0.10, colour = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(aes(shape = group), size = 3) +
  scale_colour_manual(values = col_sex, name = "Gender") +
  scale_fill_manual(values   = col_sex, name = "Gender") +
  scale_linetype_manual(values = c(EU27 = "solid", Italy = "dashed"),
                        name  = "Group") +
  scale_shape_manual(values  = c(EU27 = 16, Italy = 17), name = "Group") +
  scale_x_continuous(breaks  = c(2012, 2014, 2017, 2024)) +
  labs(title    = "Gender gap in robot attitudes across waves: EU27 and Italy",
       subtitle = "Weighted mean rob2item (0–6) by sex. Shaded bands = 95% CI. Solid = EU27; dashed = Italy.",
       x        = "Survey year",
       y        = "Mean composite score (rob2item, 0–6)",
       caption  = "CI bands use weighted SD; unweighted n as denominator (conservative). sex: 0 = male, 1 = female.") +
  theme_academic(base_size = 12) +
  theme(legend.position = "bottom",
        legend.box      = "horizontal")

ggsave("./plots/Figure_15_gender_gap_waves.png", p15,
       width = 11, height = 6, dpi = 150)
cat("Saved: Figure_15_gender_gap_waves.png\n")
rm(gend_eu, gend_it, gend_all, p15, compute_gender_means, col_sex)


cat("\n=== ALL FIGURES COMPLETE ===\n")
cat("Files saved in ./plots/\n")
cat(list.files("./plots/"), sep = "\n")
cat("\nScript 5 complete. Proceed to 6_Document_results_extended.R\n")
