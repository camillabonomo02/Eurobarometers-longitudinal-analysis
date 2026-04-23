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
#'   Figure 3  — Quantile regression: individual-level predictors (G&A + wave 4)
#'   Figure 4  — Longitudinal trajectory: Italy vs. EU benchmark (rob2item)
#'   Figure 5  — Scatter plot: UAI vs. composite score by country (rob2item)
#'   Figure 6  — Country random effects with Italy highlighted (rob2item; FIXED)
#'   Figure 7  — ICC decline across waves: convergence of national attitudes
#'   Figure 8  — Gender gap by country (wave 4): dot plot
#'   Figure 9  — Country rank bump chart across all four waves
#'   Figure 10 — Attitude distribution by wave: density plot (rob2item)
#'   Figure 11 — Italy vs. EU27 by demographic subgroup (wave 4)
#'   Figure 12 — EU map: attitude change wave 3 -> 4 (EXTENSION)
#'   Figure 13 — Age gradient across waves: EU27 and Italy
#'   Figure 15 — Latitude vs. composite score by country (North-South divide)
#'   Figure 16 — Gender gap across waves: EU27 and Italy
#'
#' Colour scheme (consistent across all figures):
#'   Positive / favourable / improvement -> BLUE  (#2166ac, #4575b4)
#'   Negative / unfavourable / decline   -> RED   (#d73027, #e41a1c)
#'   Neutral / mean                      -> GREY  (#f7f7f7, grey60)
#'   Italy highlight                     -> #e41a1c (red; below EU mean)
#'
#' Dependent variable note:
#'   All analyses that include wave 4 use rob2item (two-item comparable
#'   composite, range 0-6). Figures using rob (three-item, range 0-9)
#'   are restricted to waves 1-3 and labelled accordingly.


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
#' # GLOBAL COLOUR PALETTE
#' ===================================================================
#' Defined once and reused across all figures for visual consistency.

# Diverging: red (sceptical) -> white (neutral) -> blue (favourable)
pal_div <- colorRampPalette(c("#d73027", "#f46d43", "#fdae61",
                              "#f7f7f7",
                              "#abd9e9", "#74add1", "#4575b4"))

col_italy   <- "#e41a1c"   # Italy: red (below EU mean)
col_eu      <- "black"     # EU27 mean: black
col_neutral <- "grey60"    # other countries

# Wave gradient: dark blue (2012) -> orange (2017) -> red (2024)
col_waves <- c("2012" = "#4575b4",
               "2014" = "#74add1",
               "2017" = "#fdae61",
               "2024" = "#d73027")




#' ===================================================================
#' # 1. EU maps [G&A + EXTENSION wave 4]
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

  #' **Compute country-level statistics** [G&A]
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

    # Wave 4 map uses rob2item (two-item comparable composite)
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

    # Cohen's d for the AI-debate period: wave 3 (2017) -> wave 4 (2024)
    ds$delta_34[ds$cntry == i] <- tryCatch(
      ttest.imp(rob2item ~ wave_n, dati, weights = "wgt2",
                paired = FALSE, print = FALSE,
                subset = (dati[[1]]$cntry == i &
                            dati[[1]]$wave_n %in% c(3, 4)))$d * -1,
      error = function(e) NA
    )
  }

  ds$rob_w3[ds$rob_w3 < -.50] <- -.495  # floor for extreme values (G&A)

  europeanUnion <- c("Austria", "Belgium", "Bulgaria", "Cyprus",
                     "Czech Rep.", "Denmark", "Estonia", "Finland", "France",
                     "Germany", "Greece", "Croatia", "Hungary", "Ireland",
                     "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
                     "Netherlands", "Poland", "Portugal", "Romania",
                     "Slovakia", "Slovenia", "Spain", "Sweden")

  sPDF <- joinCountryData2Map(ds, joinCode = "ISO2",
                              nameJoinColumn = "cntry")
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
    mapTitle         = "Attitudes towards robots 2017 (standardized)")
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2,
                          legendLabels = "all"))
  mtext("Red = below average (more sceptical)    Blue = above average (more favourable)",
        side = 1, line = 0, cex = 0.65, col = "grey40")
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
    catMethod        = seq(-0.5, 0.1, length.out = 13),
    xlim             = bbox(sPDFmyCountries)[1, ],
    ylim             = bbox(sPDFmyCountries)[2, ],
    addLegend        = FALSE,
    colourPalette    = pal_change,
    mapTitle         = "Change in attitudes 2012-2017 (Cohen's d)")
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2,
                          legendLabels = "all"))
  mtext("Red = decline in acceptance    Blue = improvement",
        side = 1, line = 0, cex = 0.7, col = "grey40")
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
    catMethod        = seq(2.5, 4.5, length.out = 13),
    xlim             = bbox(sPDFmyCountries)[1, ],
    ylim             = bbox(sPDFmyCountries)[2, ],
    addLegend        = FALSE,
    colourPalette    = pal_score_w4,
    mapTitle         = "Attitudes towards robots and AI 2024 (rob2item, 0-6)")
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2,
                          legendLabels = "all"))
  mtext("Red = more negative    Blue = more positive",
        side = 1, line = 0, cex = 0.7, col = "grey40")
  par(op); dev.off()
  cat("Saved: Figure_1c_map_wave4.png\n")

  #' --- Figure 12: attitude change 2017 -> 2024 (rob2item) [EXTENSION] ---
  #' Covers the AI-debate period following the rapid diffusion of large language
  #' models post-2020. Uses rob2item throughout for cross-wave comparability.
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
    mapTitle         = "Change in attitudes 2017\u20132024 (Cohen\u2019s d, rob2item)")
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2,
                          legendLabels = "all"))
  mtext("Red = decline in acceptance    Blue = improvement (2017 \u2192 2024)",
        side = 1, line = 0, cex = 0.7, col = "grey40")
  par(op); dev.off()
  cat("Saved: Figure_12_map_change34.png\n")

  rm(rob_w3_mean, rob_w3_sd, ds, mapParams, op, sPDF,
     sPDFmyCountries, europeanUnion, paesi, i,
     pal_score, pal_change, pal_score_w4, pal_change_34)

} else {
  cat("rworldmap not available. Install with install.packages('rworldmap')\n")
  cat("Figures 1a-c skipped.\n")
}




#' ===================================================================
#' # 2. Bar chart — mean attitudes by wave [G&A, waves 1-3 only]
#' ===================================================================
#' Wave 4 is excluded because rob3 wording changed in 2024.
#' The longitudinal comparison including wave 4 is presented in Figure 4.

cat("\n=== FIGURE 2: MEAN ATTITUDES BY WAVE (1-3, G&A replication) ===\n")

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
               "Assisting\nat work",  "Services\nfor elderly",
               "Driverless\ncars"), 3),
  mean = c(
    w1["rob",   "mean"], w1["feel1", "mean"], w1["feel2", "mean"], NA, NA,
    w2["rob",   "mean"], w2["feel1", "mean"], w2["feel2", "mean"],
    w2["feel3", "mean"], w2["feel4", "mean"],
    w3["rob",   "mean"], w3["feel1", "mean"], w3["feel2", "mean"],
    w3["feel3", "mean"], w3["feel4", "mean"]
  ),
  sd = c(
    w1["rob",   "sd"],   w1["feel1", "sd"],   w1["feel2", "sd"],   NA, NA,
    w2["rob",   "sd"],   w2["feel1", "sd"],   w2["feel2", "sd"],
    w2["feel3", "sd"],   w2["feel4", "sd"],
    w3["rob",   "sd"],   w3["feel1", "sd"],   w3["feel2", "sd"],
    w3["feel3", "sd"],   w3["feel4", "sd"]
  )
)
plot_data$wave <- factor(plot_data$wave, levels = c("2012", "2014", "2017"))
plot_data$item <- factor(plot_data$item,
                         levels = c("General\nappraisal", "Medical\noperation",
                                    "Assisting\nat work", "Services\nfor elderly",
                                    "Driverless\ncars"))

p2 <- ggplot(plot_data[!is.na(plot_data$mean), ],
             aes(x = item, y = mean, fill = wave)) +
  geom_bar(stat = "identity", position = position_dodge(0.8),
           width = 0.7, colour = "grey30", linewidth = 0.2) +
  geom_errorbar(aes(ymin = mean - sd / 2, ymax = mean + sd / 2),
                position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(values = c("2012" = "#4575b4",
                               "2014" = "#74add1",
                               "2017" = "#fdae61"),
                    name = "Wave") +
  scale_y_continuous(limits = c(0, 9), breaks = 0:9) +
  labs(title = "Mean attitudes towards robots in Europe (2012\u20132017)",
       subtitle = paste("Weighted means, EU27.",
                        "Wave 2024 excluded (rob3 item non-comparable; see Fig. 4)."),
       x = NULL,
       y = "Mean attitude score (0\u20139)") +
  theme_minimal(base_size = 13) +
  theme(legend.position  = "bottom",
        panel.grid.major.x = element_blank())

ggsave("./plots/Figure_2_attitudes_by_wave.png", p2,
       width = 12, height = 7, dpi = 150)
cat("Saved: Figure_2_attitudes_by_wave.png\n")
rm(w1, w2, w3, plot_data, p2)




#' ===================================================================
#' # 3. Quantile regression [G&A + EXTENSION wave 4]
#' ===================================================================

d1 <- dati[[1]]
d1$white  <- as.factor(d1$white)
d1$sex    <- as.factor(d1$sex)
d1$wave   <- as.factor(d1$wave)
d1$age_s  <- as.numeric(d1$age) / 10
d1$zrob   <- as.numeric(scale(d1$rob))

#' --- Waves 1-3 (G&A replication) ---
d1_123 <- d1[d1$wave_n %in% 1:3, ]
fm_plot <- rq(zrob ~ wave + sex + age_s + educ + white,
              tau = c(.25, .50, .75), data = d1_123,
              weights = d1_123$wgt2)

png("./plots/Figure_3a_quantreg_wave123.png",
    width = 12, height = 5, units = "in", res = 150)
plot(summary(fm_plot, se = "nid"), ols = FALSE,
     parm = 4:8, mfrow = c(2, 3),
     main = c("Male vs. female", "Age (10 yr)", "Education",
              "White- vs. blue-collar", "White-collar vs. non-employed"),
     xlab = "Percentile of attitude rating",
     ylab = "Standardized regression weight")
dev.off()
cat("Saved: Figure_3a_quantreg_wave123.png\n")

#' --- Waves 1-4 (EXTENSION) ---
#' rob is used here for continuity with the quantile regression framework;
#' note that wave 4 includes a non-comparable rob3 item.
fm_plot4 <- rq(zrob ~ wave + sex + age_s + educ + white,
               tau = c(.25, .50, .75), data = d1,
               weights = d1$wgt2)

png("./plots/Figure_3b_quantreg_wave1234.png",
    width = 12, height = 5, units = "in", res = 150)
plot(summary(fm_plot4, se = "nid"), ols = FALSE,
     parm = 5:9, mfrow = c(2, 3),
     main = c("Male vs. female", "Age (10 yr)", "Education",
              "White- vs. blue-collar", "White-collar vs. non-employed"),
     xlab = "Percentile of attitude rating",
     ylab = "Standardized regression weight")
dev.off()
cat("Saved: Figure_3b_quantreg_wave1234.png\n")
rm(d1_123, fm_plot, fm_plot4)




#' ===================================================================
#' # 4. Longitudinal trajectory — Italy vs. EU benchmark [EXTENSION]
#' ===================================================================
#' rob2item (two-item comparable composite, range 0-6) used throughout.

cat("\n=== FIGURE 4: LONGITUDINAL TRAJECTORY (rob2item) ===\n")

benchmark_countries <- c("IT", "DE", "FR", "DK", "SE", "GR", "PT", "ES")
anni <- c(2012, 2014, 2017, 2024)

traj_data <- do.call(rbind, lapply(benchmark_countries, function(cc) {
  do.call(rbind, lapply(1:4, function(w) {
    sub <- dat[dat$cntry == cc & dat$wave == w &
                 !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
    if (nrow(sub) == 0) return(NULL)
    data.frame(cntry = cc, anno = anni[w], wave = w,
               mean = weighted.mean(sub$rob2item, sub$wgt2), n = nrow(sub))
  }))
}))

eu_traj <- do.call(rbind, lapply(1:4, function(w) {
  sub <- dat[dat$wave == w & !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
  data.frame(cntry = "EU27", anno = anni[w], wave = w,
             mean = weighted.mean(sub$rob2item, sub$wgt2), n = nrow(sub))
}))
traj_data <- rbind(traj_data, eu_traj)

paese_colors <- c(IT = col_italy, DE = "#377eb8", FR = "#4daf4a",
                  DK = "#984ea3", SE = "#ff7f00", GR = "#a65628",
                  PT = "#f781bf", ES = "#999999", EU27 = col_eu)
paese_types  <- c(IT = "solid", DE = "dashed", FR = "dashed",
                  DK = "dotted", SE = "dotted", GR = "dashed",
                  PT = "dotted", ES = "dotted", EU27 = "solid")
paese_sizes  <- c(IT = 1.5, DE = 0.8, FR = 0.8, DK = 0.8, SE = 0.8,
                  GR = 0.8, PT = 0.8, ES = 0.8, EU27 = 1.2)

traj_data$cntry <- factor(traj_data$cntry,
                          levels = c("IT", "EU27", "DE", "FR", "DK",
                                     "SE", "GR", "PT", "ES"))

p4 <- ggplot(traj_data, aes(x = anno, y = mean,
                            colour = cntry, linetype = cntry,
                            linewidth = cntry)) +
  geom_line() +
  geom_point(size = 2) +
  scale_colour_manual(values = paese_colors,  name = "Country") +
  scale_linetype_manual(values = paese_types, name = "Country") +
  scale_linewidth_manual(values = paese_sizes, name = "Country") +
  scale_x_continuous(breaks = c(2012, 2014, 2017, 2024)) +
  scale_y_continuous(breaks = seq(2, 6, 0.5)) +
  geom_text(data = subset(traj_data, anno == 2024 & cntry == "IT"),
            aes(label = "Italy"), nudge_x = 0.5, nudge_y = 0.02,
            colour = col_italy, size = 3.5, fontface = "bold") +
  geom_text(data = subset(traj_data, anno == 2024 & cntry == "EU27"),
            aes(label = "EU27"), nudge_x = 0.5,
            colour = col_eu, size = 3.5) +
  labs(title = "Attitudes toward robots and AI: Italy vs. EU benchmark (2012\u20132024)",
       subtitle = "Weighted composite score (rob2item, two comparable items, 0\u20136)",
       x = "Year", y = "Mean composite score (rob2item, 0\u20136)") +
  theme_minimal(base_size = 13) +
  theme(legend.position  = "right",
        panel.grid.minor = element_blank())

ggsave("./plots/Figure_4_italy_trajectory.png", p4,
       width = 11, height = 6, dpi = 150)
cat("Saved: Figure_4_italy_trajectory.png\n")
rm(traj_data, eu_traj, p4, benchmark_countries)




#' ===================================================================
#' # 5. UAI scatter — by country [EXTENSION]
#' ===================================================================
#' rob2item used for both panels (waves 3 and 4).

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
  data.frame(cntry = scatter_data$cntry, UAI = scatter_data$UAI,
             score = scatter_data$score_w3,
             wave = "2017 (rob2item)", is_italy = scatter_data$is_italy),
  data.frame(cntry = scatter_data$cntry, UAI = scatter_data$UAI,
             score = scatter_data$score_w4,
             wave = "2024 (rob2item)", is_italy = scatter_data$is_italy)
)
scatter_long <- scatter_long[!is.na(scatter_long$score) &
                               !is.na(scatter_long$UAI), ]

r_labels <- data.frame(
  wave  = c("2017 (rob2item)", "2024 (rob2item)"),
  label = c(sprintf("r = %.3f", r_2017), sprintf("r = %.3f", r_2024)),
  UAI   = 100,
  score = max(scatter_long$score, na.rm = TRUE) * 0.95
)

p5 <- ggplot(scatter_long, aes(x = UAI, y = score, colour = is_italy)) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey50",
              fill = "grey85", linewidth = 0.7, linetype = "dashed") +
  geom_point(aes(size = is_italy)) +
  geom_text_repel(aes(label = cntry),
                  size = 2.8, max.overlaps = 20, segment.size = 0.2) +
  geom_text(data = r_labels, aes(x = UAI, y = score, label = label),
            inherit.aes = FALSE, hjust = 1, vjust = 1,
            size = 4, fontface = "italic", colour = "grey30") +
  scale_colour_manual(values = c("FALSE" = col_neutral, "TRUE" = col_italy),
                      labels = c("EU countries", "Italy"), name = NULL) +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4), guide = "none") +
  facet_wrap(~wave, scales = "free_y") +
  labs(title = "Uncertainty Avoidance Index vs. robot attitudes by country",
       subtitle = "Each point = one EU country; dashed line = OLS fit with 95% CI",
       x = "Hofstede Uncertainty Avoidance Index (UAI)",
       y = "Weighted mean composite score (rob2item, 0\u20136)") +
  theme_minimal(base_size = 13) +
  theme(strip.text       = element_text(size = 13, face = "bold"),
        legend.position  = "bottom")

ggsave("./plots/Figure_5_UAI_scatter.png", p5,
       width = 12, height = 6, dpi = 150)
cat("Saved: Figure_5_UAI_scatter.png\n")
rm(scatter_data, scatter_long, r_labels, r_2017, r_2024, p5)




#' ===================================================================
#' # 6. Country random effects with Italy highlighted [EXTENSION — FIXED]
#' ===================================================================
#' METHODOLOGICAL FIX relative to earlier versions:
#'   Models for waves 1-4 now use rob2item (two-item comparable composite)
#'   rather than rob, which included a non-comparable rob3 item in wave 4.
#'   Random effects are averaged across all m=20 imputed datasets.

cat("\n=== FIGURE 6: COUNTRY RANDOM EFFECTS ===\n")

#' **Re-prepare imputed data with required transformations**
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
uai_sd   <- sd(unique(dati[[1]][, c("cid", "UAI")])$UAI,   na.rm = TRUE)
dati <- lapply(dati, function(x) {
  x$UAI_z <- (x$UAI - uai_mean) / uai_sd
  x
})
dati <- as.mitml.list(dati)

#' **Fit Models A2 and B2 (waves 1-4) across all 20 imputed datasets**
#' rob2item is used as the dependent variable throughout.
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

#' **Pool random effects by averaging BLUPs across imputed datasets**
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
  data.frame(cntry = re_A2$cntry, re = re_A2$re_A2,
             model = "A2 (without UAI)"),
  data.frame(cntry = re_B2$cntry, re = re_B2$re_B2,
             model = "B2 (with UAI)")
)
re_long$is_italy <- re_long$cntry == "IT"

ord <- re_A2$cntry[order(re_A2$re_A2)]
re_long$cntry <- factor(re_long$cntry, levels = ord)

p6 <- ggplot(re_long, aes(x = cntry, y = re)) +
  geom_hline(yintercept = 0, colour = "grey30", linetype = "dashed",
             alpha = 0.5) +
  geom_line(aes(group = cntry, colour = is_italy),
            linewidth = 0.6, alpha = 0.5) +
  geom_point(aes(colour = is_italy, shape = model), size = 2.5) +
  scale_colour_manual(values = c("FALSE" = col_neutral, "TRUE" = col_italy),
                      labels = c("Other EU countries", "Italy"), name = NULL) +
  scale_shape_manual(values = c("A2 (without UAI)" = 16, "B2 (with UAI)" = 17),
                     name = "Model") +
  annotate("text", x = which(levels(re_long$cntry) == "IT"),
           y = min(re_long$re) - 0.06,
           label = "Italy", colour = col_italy,
           size = 3.5, fontface = "bold") +
  annotate("text", x = length(levels(re_long$cntry)) - 1,
           y = max(re_long$re) + 0.04,
           label = "Above model\nprediction",
           colour = "#4575b4", size = 3, fontface = "italic", hjust = 0.5) +
  annotate("text", x = 2,
           y = min(re_long$re) + 0.04,
           label = "Below model\nprediction",
           colour = "#d73027", size = 3, fontface = "italic", hjust = 0.5) +
  labs(title = "Country random effects: Model A2 vs. B2 (waves 1\u20134, rob2item)",
       subtitle = paste("Lines connect A2 \u2192 B2 for each country.",
                        "Negative = more sceptical than structural predictors expect."),
       x = "Country", y = "Random intercept") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x    = element_text(angle = 45, hjust = 1, size = 9),
        legend.position = "bottom",
        legend.box      = "horizontal")

ggsave("./plots/Figure_6_random_effects.png", p6,
       width = 13, height = 6, dpi = 150)
cat("Saved: Figure_6_random_effects.png\n")
rm(m_A2_list, m_B2_list, re_A2, re_B2, re_long, ord, p6,
   uai_mean, uai_sd, cid_cntry)




#' ===================================================================
#' # 7. ICC decline across waves — convergence [EXTENSION]
#' ===================================================================
#' A declining ICC indicates that country membership explains a
#' progressively smaller proportion of individual-level variance,
#' consistent with homogenisation of attitudes across the EU.
#' Wave 4 ICC is computed on rob2item for comparability.

cat("\n=== FIGURE 7: ICC DECLINE ACROSS WAVES ===\n")

icc_vals <- do.call(rbind, lapply(1:4, function(w) {
  outcome <- if (w <= 3) "rob" else "rob2item"
  vars <- sapply(dati, function(x) {
    sub <- x[x$wave_n == w, ]
    fit <- lmer(as.formula(paste(outcome, "~ 1 + (1 | cid)")),
                data = sub, weights = sub$wgt2,
                control = lmerControl(optimizer = "nloptwrap"))
    vc <- as.data.frame(VarCorr(fit))
    v_cid <- vc$vcov[vc$grp == "cid"]
    v_res <- vc$vcov[vc$grp == "Residual"]
    v_cid / (v_cid + v_res)
  })
  data.frame(
    wave  = c(2012, 2014, 2017, 2024)[w],
    ICC   = mean(vars),
    scale = if (w <= 3) "rob (0\u20139)" else "rob2item (0\u20136)"
  )
}))

p7 <- ggplot(icc_vals, aes(x = wave, y = ICC)) +
  geom_col(fill = "#4575b4", width = 1.5, alpha = 0.8, colour = "white") +
  geom_line(colour = "#d73027", linewidth = 1.2) +
  geom_point(colour = "#d73027", size = 4) +
  geom_text(aes(label = sprintf("%.3f", ICC), y = ICC + 0.003),
            size = 4.5, fontface = "bold", colour = "grey20") +
  geom_text(aes(label = scale, y = -0.004),
            size = 3, colour = "grey50") +
  scale_x_continuous(breaks = c(2012, 2014, 2017, 2024)) +
  scale_y_continuous(limits = c(-0.008, 0.12),
                     breaks = seq(0, 0.12, 0.02),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Between-country variance (ICC) across waves: convergence of EU attitudes",
       subtitle = paste("ICC = proportion of attitude variance attributable to country.",
                        "Wave 4 uses rob2item (0\u20136); waves 1\u20133 use rob (0\u20139)."),
       x = "Survey year", y = "ICC (% variance explained by country)") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank())

ggsave("./plots/Figure_7_icc_convergence.png", p7,
       width = 9, height = 6, dpi = 150)
cat("Saved: Figure_7_icc_convergence.png\n")
rm(icc_vals, p7)




#' ===================================================================
#' # 8. Gender gap by country — wave 4 [EXTENSION]
#' ===================================================================
#' For each EU country, the gender gap is computed as the difference
#' in weighted mean rob2item between female and male respondents.
#' A negative value indicates that women hold more sceptical attitudes.
#' Italy is highlighted to examine whether the EU-wide gender pattern
#' holds within the Italian subsample.

cat("\n=== FIGURE 8: GENDER GAP BY COUNTRY (wave 4, rob2item) ===\n")

gap_data <- do.call(rbind, lapply(unique(dat$cntry), function(cc) {
  sub <- dat[dat$cntry == cc & dat$wave == 4 &
               !is.na(dat$rob2item) & !is.na(dat$sex) &
               !is.na(dat$wgt2), ]
  sub_f <- sub[sub$sex == 1, ]
  sub_m <- sub[sub$sex == 0, ]
  if (nrow(sub_f) < 10 || nrow(sub_m) < 10) return(NULL)
  m_f <- weighted.mean(sub_f$rob2item, sub_f$wgt2)
  m_m <- weighted.mean(sub_m$rob2item, sub_m$wgt2)
  data.frame(cntry = cc, gap = m_f - m_m,
             m_female = m_f, m_male = m_m,
             is_italy = cc == "IT")
}))

gap_data <- gap_data[order(gap_data$gap), ]
gap_data$cntry <- factor(gap_data$cntry, levels = gap_data$cntry)

p8 <- ggplot(gap_data, aes(x = gap, y = cntry, colour = is_italy)) +
  geom_vline(xintercept = 0, colour = "grey40", linetype = "dashed") +
  geom_segment(aes(x = 0, xend = gap, y = cntry, yend = cntry,
                   colour = is_italy), linewidth = 0.8) +
  geom_point(aes(size = is_italy)) +
  geom_text(data = subset(gap_data, is_italy),
            aes(label = "Italy"), nudge_y = 0.5, nudge_x = 0.01,
            colour = col_italy, size = 3.5, fontface = "bold") +
  scale_colour_manual(values = c("FALSE" = col_neutral, "TRUE" = col_italy),
                      guide = "none") +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4), guide = "none") +
  scale_x_continuous(breaks = seq(-0.8, 0.4, 0.2)) +
  labs(title = "Gender gap in robot attitudes by country (wave 4, 2024)",
       subtitle = paste("Gap = women\u2019s mean \u2212 men\u2019s mean (rob2item, 0\u20136).",
                        "Negative = women more sceptical."),
       x = "Gender gap (female \u2212 male)", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey85"),
        axis.text.y        = element_text(size = 9))

ggsave("./plots/Figure_8_gender_gap_by_country.png", p8,
       width = 10, height = 9, dpi = 150)
cat("Saved: Figure_8_gender_gap_by_country.png\n")
rm(gap_data, p8)




#' ===================================================================
#' # 9. Country rank bump chart across waves [EXTENSION]
#' ===================================================================
#' Ranks countries by mean rob2item within each wave. Selected countries
#' (Italy, Germany, France, Denmark, Sweden, Greece, EU25 median) are
#' highlighted to show rank mobility. Rank 1 = most favourable.

cat("\n=== FIGURE 9: COUNTRY RANK EVOLUTION ACROSS WAVES ===\n")

anni <- c(2012, 2014, 2017, 2024)

country_means <- do.call(rbind, lapply(unique(dat$cntry), function(cc) {
  do.call(rbind, lapply(1:4, function(w) {
    sub <- dat[dat$cntry == cc & dat$wave == w &
                 !is.na(dat$rob2item) & !is.na(dat$wgt2), ]
    if (nrow(sub) == 0) return(NULL)
    data.frame(cntry = cc, wave = w, anno = anni[w],
               mean_rob2 = weighted.mean(sub$rob2item, sub$wgt2))
  }))
}))

country_means <- country_means %>%
  group_by(wave) %>%
  mutate(rank = rank(-mean_rob2, ties.method = "average")) %>%
  ungroup()

highlight <- c("IT", "DE", "DK", "SE", "GR", "FR")
country_means$highlight <- country_means$cntry %in% highlight
country_means$label_cntry <- ifelse(country_means$highlight,
                                    country_means$cntry, "")

hl_colors <- c(IT = col_italy, DE = "#377eb8", DK = "#984ea3",
               SE = "#ff7f00", GR = "#a65628", FR = "#4daf4a")

p9 <- ggplot(country_means, aes(x = anno, y = rank, group = cntry)) +
  geom_line(data = subset(country_means, !highlight),
            colour = "grey80", linewidth = 0.4, alpha = 0.7) +
  geom_line(data = subset(country_means, highlight),
            aes(colour = cntry), linewidth = 1.2) +
  geom_point(data = subset(country_means, highlight),
             aes(colour = cntry), size = 2.5) +
  geom_text(data = subset(country_means, highlight & anno == 2024),
            aes(label = cntry, colour = cntry),
            nudge_x = 0.8, size = 3.5, fontface = "bold") +
  geom_text(data = subset(country_means, highlight & anno == 2012),
            aes(label = cntry, colour = cntry),
            nudge_x = -0.8, size = 3.5, fontface = "bold") +
  scale_colour_manual(values = hl_colors, guide = "none") +
  scale_x_continuous(breaks = c(2012, 2014, 2017, 2024),
                     expand = expansion(mult = 0.15)) +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 27),
                  labels = c("1st", "5th", "10th", "15th", "20th", "27th")) +
  labs(title = "Country rankings in robot acceptance across waves (rob2item)",
       subtitle = "Rank 1 = most favourable. Grey lines = all EU27 countries.",
       x = "Survey year", y = "Country rank (1 = most favourable)") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

ggsave("./plots/Figure_9_country_rank_bumpchart.png", p9,
       width = 11, height = 8, dpi = 150)
cat("Saved: Figure_9_country_rank_bumpchart.png\n")
rm(country_means, p9)




#' ===================================================================
#' # 10. Attitude distribution by wave — density plot [EXTENSION]
#' ===================================================================
#' Visualises the full distribution of rob2item across all four waves,
#' capturing not only mean shifts but changes in distributional shape.
#' A universal leftward shift would confirm the rising scepticism finding.

cat("\n=== FIGURE 10: ATTITUDE DISTRIBUTION BY WAVE ===\n")

dist_data <- dat[!is.na(dat$rob2item) & !is.na(dat$wgt2), ]
dist_data$Year <- factor(c("2012", "2014", "2017", "2024")[dist_data$wave],
                         levels = c("2012", "2014", "2017", "2024"))

# Weighted proportion per score x wave (for interpretable bar chart overlay)
prop_tab <- dist_data %>%
  group_by(Year, rob2item) %>%
  summarise(sum_wgt = sum(wgt2), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(prop = sum_wgt / sum(sum_wgt)) %>%
  ungroup()

p10 <- ggplot(prop_tab, aes(x = rob2item, y = prop, fill = Year, colour = Year)) +
  geom_col(position = "dodge", alpha = 0.7, width = 0.7) +
  scale_fill_manual(values   = col_waves, name = "Wave") +
  scale_colour_manual(values = col_waves, name = "Wave") +
  scale_x_continuous(breaks = 0:6,
                     labels = c("0\n(very neg.)", "1", "2", "3",
                                "4", "5", "6\n(very pos.)")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Distribution of robot attitudes across waves (rob2item, 0\u20136)",
       subtitle = "Weighted proportions by score value, EU27",
       x = "Composite score (rob2item)", y = "Proportion of respondents") +
  theme_minimal(base_size = 13) +
  theme(legend.position   = "bottom",
        panel.grid.major.x = element_blank())

ggsave("./plots/Figure_10_attitude_distribution.png", p10,
       width = 11, height = 6, dpi = 150)
cat("Saved: Figure_10_attitude_distribution.png\n")
rm(dist_data, prop_tab, p10)




#' ===================================================================
#' # 11. Italy vs. EU27 by demographic subgroup — wave 4 [EXTENSION]
#' ===================================================================
#' Tests whether Italy's attitude deficit is uniform across all
#' sociodemographic groups or concentrated in specific subgroups.
#' If the deficit is disproportionately large among, for example,
#' educated women or white-collar workers, this points to mechanisms
#' that operate differently within Italy than in the rest of the EU.

cat("\n=== FIGURE 11: ITALY VS. EU27 BY SUBGROUP (wave 4) ===\n")

sub4 <- dat[dat$wave == 4 & !is.na(dat$rob2item) &
              !is.na(dat$sex) & !is.na(dat$educ) &
              !is.na(dat$white) & !is.na(dat$wgt2), ]

# Education tertiles computed on the full EU27 wave 4 sample
educ_breaks <- quantile(sub4$educ, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
sub4$educ_tert <- cut(sub4$educ, breaks = educ_breaks,
                      labels = c("Low\neducation", "Medium\neducation",
                                 "High\neducation"),
                      include.lowest = TRUE)
sub4$sex_label <- factor(ifelse(sub4$sex == 0, "Male", "Female"),
                         levels = c("Male", "Female"))
sub4$empl_label <- factor(
  c("White-collar", "Blue-collar", "Non-employed")[as.integer(sub4$white)],
  levels = c("White-collar", "Blue-collar", "Non-employed"))
sub4$group <- ifelse(sub4$cntry == "IT", "Italy", "EU27")

# Compute weighted means for sex x education subgroups
sg_edu <- sub4 %>%
  filter(!is.na(educ_tert)) %>%
  group_by(group, sex_label, educ_tert) %>%
  summarise(mean_rob = weighted.mean(rob2item, wgt2, na.rm = TRUE),
            .groups = "drop")

# Compute weighted means for employment subgroups
sg_empl <- sub4 %>%
  filter(!is.na(empl_label)) %>%
  group_by(group, empl_label) %>%
  summarise(mean_rob = weighted.mean(rob2item, wgt2, na.rm = TRUE),
            .groups = "drop") %>%
  rename(subgroup = empl_label)

sg_edu2 <- sg_edu %>%
  mutate(subgroup = interaction(sex_label, educ_tert, sep = "\n")) %>%
  select(group, subgroup, mean_rob)

sg_all <- rbind(sg_edu2, sg_empl)
sg_all$group <- factor(sg_all$group, levels = c("EU27", "Italy"))

p11 <- ggplot(sg_all, aes(x = subgroup, y = mean_rob,
                           colour = group, group = group)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(aes(shape = group), size = 3) +
  scale_colour_manual(values = c("EU27" = col_eu, "Italy" = col_italy),
                      name = NULL) +
  scale_shape_manual(values = c("EU27" = 16, "Italy" = 17), name = NULL) +
  facet_wrap(~sub("\\n.*", "", subgroup),
             scales = "free_x", nrow = 1, strip.position = "bottom") +
  labs(title = "Italy vs. EU27: robot attitudes by demographic subgroup (wave 4, 2024)",
       subtitle = "Weighted mean rob2item (0\u20136). Each panel groups a different dimension.",
       x = NULL, y = "Mean composite score (rob2item, 0\u20136)") +
  theme_minimal(base_size = 12) +
  theme(legend.position   = "top",
        axis.text.x       = element_text(size = 8, angle = 30, hjust = 1),
        panel.grid.minor  = element_blank(),
        strip.placement   = "outside")

ggsave("./plots/Figure_11_italy_subgroup_profile.png", p11,
       width = 13, height = 6, dpi = 150)
cat("Saved: Figure_11_italy_subgroup_profile.png\n")
rm(sub4, sg_edu, sg_empl, sg_edu2, sg_all, p11)




#' ===================================================================
#' # 13. Age gradient across waves: EU27 and Italy [EXTENSION]
#' ===================================================================
#' Tests whether Italy's 2024 finding — age as the only robust predictor —
#' reflects a broader EU trend or an Italy-specific feature.
#' Age tertiles are defined globally (pooled across all waves and countries)
#' to ensure comparability of the gradient over time.

cat("\n=== FIGURE 13: AGE GRADIENT ACROSS WAVES ===\n")

age_breaks <- quantile(dat$age, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
cat(sprintf("  Age tertile boundaries: <=%.0f | %.0f-%.0f | >%.0f\n",
            age_breaks[2], age_breaks[2], age_breaks[3], age_breaks[3]))

dat$age_tert <- cut(dat$age,
                    breaks         = age_breaks,
                    labels         = c("Young", "Middle-aged", "Older"),
                    include.lowest = TRUE)

anni <- c(2012, 2014, 2017, 2024)

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

p13 <- ggplot(age_data, aes(x = anno, y = mean,
                             colour   = age_group,
                             linetype = age_group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  facet_wrap(~ group, ncol = 2) +
  scale_x_continuous(breaks = c(2012, 2014, 2017, 2024)) +
  scale_colour_manual(
    values = c(Young = "#4575b4", "Middle-aged" = "#fdae61", Older = "#d73027"),
    name   = "Age group") +
  scale_linetype_manual(
    values = c(Young = "solid", "Middle-aged" = "dashed", Older = "dotted"),
    name   = "Age group") +
  labs(
    title    = "Age gradient in robot attitudes across waves: EU27 and Italy",
    subtitle = paste("Weighted mean rob2item (0\u20136) by age tertile.",
                     "Tertiles defined on the pooled EU27 sample."),
    x = "Survey year",
    y = "Mean composite score (rob2item, 0\u20136)") +
  theme_minimal(base_size = 13) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        strip.text       = element_text(size = 13, face = "bold"))

ggsave("./plots/Figure_13_age_gradient.png", p13,
       width = 12, height = 6, dpi = 150)
cat("Saved: Figure_13_age_gradient.png\n")
rm(age_breaks, age_data, p13)




#' ===================================================================
#' # 15. Latitude vs. composite score by country [EXTENSION]
#' ===================================================================
#' Visualises the North-South gradient in robot acceptance that emerges
#' as a significant country-level predictor in Model A2 (b = +0.045***,
#' d = +0.32). Presented side-by-side for waves 3 and 4 to examine
#' whether the geographic gradient persists into the AI-debate era.

cat("\n=== FIGURE 15: LATITUDE vs. COMPOSITE SCORE ===\n")

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

r_labels15 <- data.frame(
  wave_label = c("2017", "2024"),
  label      = c(sprintf("r = %.3f", r_lat["2017"]),
                 sprintf("r = %.3f", r_lat["2024"])),
  LAT        = min(lat_data$LAT, na.rm = TRUE) + 1,
  mean_rob   = max(lat_data$mean_rob, na.rm = TRUE) * 0.985
)

p15 <- ggplot(lat_data, aes(x = LAT, y = mean_rob)) +
  geom_smooth(method = "lm", se = TRUE,
              inherit.aes = FALSE,
              mapping      = aes(x = LAT, y = mean_rob),
              colour = "grey50", fill = "grey85",
              linewidth = 0.8, linetype = "dashed") +
  geom_point(aes(colour = is_italy, size = is_italy)) +
  geom_text_repel(aes(label = cntry, colour = is_italy),
                  size = 2.8, max.overlaps = 20, segment.size = 0.2) +
  geom_text(data        = r_labels15,
            mapping      = aes(x = LAT, y = mean_rob, label = label),
            inherit.aes = FALSE,
            hjust = 0, size = 4, fontface = "italic", colour = "grey30") +
  scale_colour_manual(
    values = c("FALSE" = col_neutral, "TRUE" = col_italy),
    labels = c("EU countries", "Italy"), name = NULL) +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4), guide = "none") +
  facet_wrap(~ wave_label, ncol = 2) +
  labs(
    title    = "North-South gradient in robot acceptance: latitude vs. country mean",
    subtitle = "Weighted mean rob2item (0\u20136) by country; dashed line = OLS fit with 95% CI",
    x        = "Country latitude (degrees North)",
    y        = "Weighted mean composite score (rob2item, 0\u20136)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        strip.text      = element_text(size = 13, face = "bold"))

ggsave("./plots/Figure_15_latitude_gradient.png", p15,
       width = 12, height = 6, dpi = 150)
cat("Saved: Figure_15_latitude_gradient.png\n")
rm(lat_data, r_lat, r_labels15, p15)




#' ===================================================================
#' # 16. Gender gap across waves: EU27 and Italy [EXTENSION]
#' ===================================================================
#' Places the wave 4 finding — gender gap absent in Italy (b = +0.033 ns)
#' versus a persistent EU gap (b = -0.326***) — in longitudinal perspective.
#' Shows whether Italy's pattern is a recent reversal or a long-standing
#' feature of its attitude structure. sex = 1: female; sex = 0: male.

cat("\n=== FIGURE 16: GENDER GAP ACROSS WAVES (EU27 and Italy) ===\n")

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

gend_eu <- compute_gender_means(dat, "EU27")
gend_it <- compute_gender_means(dat[dat$cntry == "IT", ], "Italy")
gend_all <- rbind(gend_eu, gend_it)
gend_all$sex   <- factor(gend_all$sex,   levels = c("Male", "Female"))
gend_all$group <- factor(gend_all$group, levels = c("EU27", "Italy"))

for (grp in c("EU27", "Italy")) {
  cat(sprintf("\n  %s gender gap (Female - Male) by wave:\n", grp))
  sub_g <- gend_all[gend_all$group == grp, ]
  for (yr in c(2012, 2014, 2017, 2024)) {
    mf <- sub_g$mean[sub_g$anno == yr & sub_g$sex == "Female"]
    mm <- sub_g$mean[sub_g$anno == yr & sub_g$sex == "Male"]
    if (length(mf) > 0 && length(mm) > 0)
      cat(sprintf("    %d: gap = %+.3f\n", yr, mf - mm))
  }
}

p16 <- ggplot(gend_all,
              aes(x     = anno,
                  y     = mean,
                  colour = sex,
                  linetype = group,
                  group  = interaction(sex, group))) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex),
              alpha = 0.08, colour = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(aes(shape = group), size = 3) +
  scale_colour_manual(
    values = c(Male = "#4575b4", Female = "#d73027"), name = "Gender") +
  scale_fill_manual(
    values = c(Male = "#4575b4", Female = "#d73027"), name = "Gender") +
  scale_linetype_manual(
    values = c(EU27 = "solid", Italy = "dashed"),  name = "Group") +
  scale_shape_manual(
    values = c(EU27 = 16, Italy = 17), name = "Group") +
  scale_x_continuous(breaks = c(2012, 2014, 2017, 2024)) +
  labs(
    title    = "Gender gap in robot attitudes across waves: EU27 and Italy",
    subtitle = paste("Weighted mean rob2item (0\u20136) by sex.",
                     "Shaded bands = 95% CI. Solid = EU27; dashed = Italy."),
    x = "Survey year",
    y = "Mean composite score (rob2item, 0\u20136)") +
  theme_minimal(base_size = 13) +
  theme(legend.position  = "bottom",
        legend.box       = "horizontal",
        panel.grid.minor = element_blank())

ggsave("./plots/Figure_16_gender_gap_waves.png", p16,
       width = 11, height = 6, dpi = 150)
cat("Saved: Figure_16_gender_gap_waves.png\n")
rm(gend_eu, gend_it, gend_all, sub_g, p16, compute_gender_means)


cat("\n=== ALL FIGURES COMPLETE ===\n")
cat("Files saved in ./plots/\n")
cat(list.files("./plots/"), sep = "\n")
cat("\nScript 5 complete. Proceed to 6_Document_results_extended.R\n")
