#' ---
#' title: Plot results — Extended (4 waves, 2012-2024)
#' author: Camilla [estende Gnambs & Appel 2019]
#' output:
#'    html_document:
#'       toc: true
#'       number_section: true
#' date: "`r Sys.time()`"
#' ---
#'
#' GRAFICI PRODOTTI:
#'   Figure 1a — Mappa EU: composite score medio per paese (wave 3, replica G&A)
#'   Figure 1b — Mappa EU: cambiamento wave 1 → 3 (replica G&A)
#'   Figure 1c — Mappa EU: composite score medio per paese (wave 4, ESTENSIONE)
#'   Figure 2  — Barplot: medie per wave e tipo di applicazione (wave 1-3 only)
#'   Figure 3  — Quantile regression: predittori individuali (replica + wave 4)
#'   Figure 4  — Traiettoria longitudinale: Italia vs benchmark EU (rob2item)
#'   Figure 5  — Scatter: UAI vs composite score per paese (rob2item)
#'   Figure 6  — Random effects per paese con highlight Italia
#'
#' SCHEMA COLORI (coerente in tutte le figure):
#'   Positivo / favorevole / miglioramento → BLU (#2166ac, "#4575b4")
#'   Negativo / sfavorevole / peggioramento → ROSSO (#d73027, "#e41a1c")
#'   Neutro / media → GRIGIO (#f7f7f7, "grey60")
#'   Italia highlight → "#e41a1c" (rosso, coerente con posizione sotto media)
#'
#' FIX rispetto alla versione precedente:
#'   - Figure 2: esclusa wave 4 (rob3 formulazione cambiata nel 2024)
#'   - Figure 4: usa rob2item (2 item comparabili, scala 0-6) per tutte le wave
#'   - Figure 5: usa rob2item per entrambi i panel (2017 e 2024)
#'   - Palette unificata: rosso = negativo, blu = positivo ovunque
#'   - Legende migliorate per tutti i grafici


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

source("./syntax/0_Start.R")

#' **Load data**
load("./data/dat.Rdata")
rm(dati_mice)

#' **Aggiungi rob2item e recode wave**
dati <- lapply(dati, function(x) {
  x$rob2item <- x$rob1 + x$rob2
  x
})
dati <- as.mitml.list(dati)

dir.create("./plots", showWarnings = FALSE)


#' ===================================================================
#' # PALETTE GLOBALE
#' ===================================================================
#' Definita una sola volta, usata ovunque per coerenza visiva.

#' Divergente: rosso (negativo) → bianco (neutro) → blu (positivo)
pal_div <- colorRampPalette(c("#d73027", "#f46d43", "#fdae61",
                              "#f7f7f7",
                              "#abd9e9", "#74add1", "#4575b4"))

#' Italia: rosso consistente (sotto media EU)
col_italy   <- "#e41a1c"
col_eu      <- "black"
col_neutral <- "grey60"

#' Wave: gradazione dal blu scuro (2012) al rosso (2024)
col_waves <- c("2012" = "#4575b4",
               "2014" = "#74add1",
               "2017" = "#fdae61",
               "2024" = "#d73027")




#' ===================================================================
#' # 1. Mappe EU [G&A + ESTENSIONE wave 4]
#' ===================================================================

maps_available <- requireNamespace("rworldmap", quietly = TRUE)

if (maps_available) {
  library(rworldmap)
  
  #' **Standardizza composite score rispetto a wave 3** [G&A]
  rob_w3_mean <- mean(sapply(dati, function(x) {
    sub <- x[x$wave == 3, ]
    weighted.mean(sub$rob, sub$wgt2, na.rm = TRUE)
  }))
  rob_w3_sd <- mean(sapply(dati, function(x) {
    sub <- x[x$wave == 3, ]
    sqrt(wtd.var(sub$rob, sub$wgt2))
  }))
  
  dati <- lapply(dati, function(x) {
    x$zrob   <- (x$rob - rob_w3_mean) / rob_w3_sd
    x$wave_n <- as.numeric(as.character(x$wave))
    x
  })
  dati <- as.mitml.list(dati)
  
  #' **Calcola statistiche per paese** [G&A]
  paesi <- unique(dat$cntry)
  ds <- data.frame(cntry = paesi, rob_w3 = NA, rob_w4 = NA,
                   delta_13 = NA, delta_14 = NA,
                   stringsAsFactors = FALSE)
  
  for (i in paesi) {
    ds$rob_w3[ds$cntry == i] <- mean(sapply(dati, function(x) {
      sub <- x[x$cntry == i & x$wave_n == 3, ]
      if (nrow(sub) > 0) weighted.mean(sub$zrob, sub$wgt2, na.rm = TRUE) else NA
    }))
    
    ds$rob_w4[ds$cntry == i] <- {
      sub <- dat[dat$cntry == i & dat$wave == 4 &
                   !is.na(dat$rob) & !is.na(dat$wgt2), ]
      if (nrow(sub) > 0) weighted.mean(sub$rob, sub$wgt2) else NA
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
        ttest.imp(rob ~ wave_n, dati, weights = "wgt2",
                  paired = FALSE, print = FALSE,
                  subset = (dati[[1]]$cntry == i &
                              dati[[1]]$wave_n %in% c(1, 4)))$d * -1
    }
  }
  
  ds$rob_w3[ds$rob_w3 < -.50] <- -.495  # Greece floor (G&A)
  
  europeanUnion <- c("Austria","Belgium","Bulgaria","Cyprus",
                     "Czech Rep.","Denmark","Estonia","Finland","France",
                     "Germany","Greece","Croatia","Hungary","Ireland",
                     "Italy","Latvia","Lithuania","Luxembourg","Malta",
                     "Netherlands","Poland","Portugal","Romania",
                     "Slovakia","Slovenia","Spain","Sweden")
  
  sPDF <- joinCountryData2Map(ds, joinCode = "ISO2",
                              nameJoinColumn = "cntry")
  sPDFmyCountries <- sPDF[sPDF$NAME %in% europeanUnion, ]
  
  #' **Palette per le mappe — schema unificato**
  #' Score: rosso (scettico, sotto media) → bianco → blu (favorevole, sopra media)
  pal_score <- pal_div(12)
  #' Cambiamento: rosso (calo) → bianco → blu (miglioramento)
  pal_change <- pal_div(12)
  #' Score wave 4 (raw, non standardizzato): stesso schema divergente
  #' centrato sul valore mediano della distribuzione (~5.5)
  pal_score_w4 <- pal_div(12)
  
  
  #' --- Figure 1a: Score wave 3 [G&A] ---
  png("./plots/Figure_1a_map_wave3.png",
      width = 7, height = 7, units = "in", res = 150)
  op <- par(fin = c(4, 10), mfcol = c(1, 1),
            mai = c(0.2, 0, 0.2, 0), xaxs = "i", yaxs = "i")
  mapParams <- mapCountryData(sPDF,
                              nameColumnToPlot = "rob_w3",
                              numCats = 12,
                              catMethod = seq(-0.5, 0.5, length.out = 13),
                              xlim = bbox(sPDFmyCountries)[1, ],
                              ylim = bbox(sPDFmyCountries)[2, ],
                              addLegend = FALSE,
                              colourPalette = pal_score,
                              mapTitle = "Attitudes towards robots 2017 (standardized)")
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2,
                          legendLabels = "all"))
  mtext("Red = below average (negative)    Blue = above average (positive)",
        side = 1, line = 0, cex = 0.7, col = "grey40")
  par(op); dev.off()
  cat("Salvato: Figure_1a_map_wave3.png\n")
  
  
  #' --- Figure 1b: Cambiamento wave 1 → 3 ---
  png("./plots/Figure_1b_map_change13.png",
      width = 7, height = 7, units = "in", res = 150)
  op <- par(fin = c(4, 10), mfcol = c(1, 1),
            mai = c(0.2, 0, 0.2, 0), xaxs = "i", yaxs = "i")
  mapParams <- mapCountryData(sPDF,
                              nameColumnToPlot = "delta_13",
                              numCats = 12,
                              catMethod = seq(-0.5, 0.1, length.out = 13),
                              xlim = bbox(sPDFmyCountries)[1, ],
                              ylim = bbox(sPDFmyCountries)[2, ],
                              addLegend = FALSE,
                              colourPalette = pal_change,
                              mapTitle = "Change in attitudes 2012-2017 (Cohen's d)")
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2,
                          legendLabels = "all"))
  mtext("Red = decline    Blue = improvement",
        side = 1, line = 0, cex = 0.7, col = "grey40")
  par(op); dev.off()
  cat("Salvato: Figure_1b_map_change13.png\n")
  
  
  #' --- Figure 1c: Score wave 4 [ESTENSIONE] ---
  png("./plots/Figure_1c_map_wave4.png",
      width = 7, height = 7, units = "in", res = 150)
  op <- par(fin = c(4, 10), mfcol = c(1, 1),
            mai = c(0.2, 0, 0.2, 0), xaxs = "i", yaxs = "i")
  mapParams <- mapCountryData(sPDF,
                              nameColumnToPlot = "rob_w4",
                              numCats = 12,
                              catMethod = seq(4.5, 7.0, length.out = 13),
                              xlim = bbox(sPDFmyCountries)[1, ],
                              ylim = bbox(sPDFmyCountries)[2, ],
                              addLegend = FALSE,
                              colourPalette = pal_score_w4,
                              mapTitle = "Attitudes towards robots and AI 2024")
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2,
                          legendLabels = "all"))
  mtext("Red = more negative    Blue = more positive",
        side = 1, line = 0, cex = 0.7, col = "grey40")
  par(op); dev.off()
  cat("Salvato: Figure_1c_map_wave4.png\n")
  
  rm(rob_w3_mean, rob_w3_sd, ds, mapParams, op, sPDF,
     sPDFmyCountries, europeanUnion, paesi, i,
     pal_score, pal_change, pal_score_w4)
  
} else {
  cat("rworldmap non disponibile. Installa con install.packages('rworldmap')\n")
  cat("Le Figure 1a-c vengono saltate.\n")
}




#' ===================================================================
#' # 2. Barplot medie per wave [G&A — solo wave 1-3]
#' ===================================================================
#' [FIX] Wave 4 esclusa: rob3 formulazione cambiata nel 2024.
#' Il confronto longitudinale 2012-2024 è nella Figure 4 (rob2item).

cat("\n=== FIGURA 2: MEDIE PER WAVE (1-3, replica G&A) ===\n")

get_wave_means <- function(wave_num, items, dati) {
  describe.imp(dati, items = items, weights = "wgt2",
               stats = c("mean", "sd"), digits = 10,
               subset = (dati[[1]]$wave_n == wave_num))
}

if (!"wave_n" %in% names(dati[[1]])) {
  dati <- lapply(dati, function(x) {
    x$wave_n <- as.numeric(as.character(x$wave))
    x
  })
  dati <- as.mitml.list(dati)
}

w1 <- get_wave_means(1, c("rob", "feel1", "feel2"), dati)
w2 <- get_wave_means(2, c("rob", "feel1", "feel2", "feel3", "feel4"), dati)
w3 <- get_wave_means(3, c("rob", "feel1", "feel2", "feel3", "feel4"), dati)

plot_data <- data.frame(
  wave  = rep(c("2012", "2014", "2017"), each = 5),
  item  = rep(c("General\nappraisal", "Medical\noperation",
                "Assisting\nat work", "Services\nfor elderly",
                "Driverless\ncars"), 3),
  mean  = c(
    w1["rob","mean"],  w1["feel1","mean"], w1["feel2","mean"], NA, NA,
    w2["rob","mean"],  w2["feel1","mean"], w2["feel2","mean"],
    w2["feel3","mean"], w2["feel4","mean"],
    w3["rob","mean"],  w3["feel1","mean"], w3["feel2","mean"],
    w3["feel3","mean"], w3["feel4","mean"]
  ),
  sd = c(
    w1["rob","sd"],    w1["feel1","sd"],   w1["feel2","sd"],   NA, NA,
    w2["rob","sd"],    w2["feel1","sd"],   w2["feel2","sd"],
    w2["feel3","sd"],   w2["feel4","sd"],
    w3["rob","sd"],    w3["feel1","sd"],   w3["feel2","sd"],
    w3["feel3","sd"],   w3["feel4","sd"]
  )
)
plot_data$wave <- factor(plot_data$wave, levels = c("2012","2014","2017"))
plot_data$item <- factor(plot_data$item,
                         levels = c("General\nappraisal", "Medical\noperation",
                                    "Assisting\nat work", "Services\nfor elderly",
                                    "Driverless\ncars"))

p2 <- ggplot(plot_data[!is.na(plot_data$mean), ],
             aes(x = item, y = mean, fill = wave)) +
  geom_bar(stat = "identity", position = position_dodge(0.8),
           width = 0.7, colour = "grey30", linewidth = 0.2) +
  geom_errorbar(aes(ymin = mean - sd/2, ymax = mean + sd/2),
                position = position_dodge(0.8), width = 0.2) +
  #' Etichetta anno sopra ogni barra
  geom_text(aes(label = wave, y = mean + sd/2 + 0.25),
            position = position_dodge(0.8),
            size = 2.8, colour = "grey30") +
  scale_fill_manual(values = c("2012" = "#4575b4",
                               "2014" = "#74add1",
                               "2017" = "#fdae61"),
                    name = "Wave") +
  scale_y_continuous(limits = c(0, 9), breaks = 0:9) +
  labs(title = "Mean attitudes towards robots in Europe (2012-2017)",
       subtitle = "Weighted means, EU27. Wave 2024 excluded (rob3 item not comparable; see Fig. 4)",
       x = NULL,
       y = "Mean attitude score (0-9)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank())

ggsave("./plots/Figure_2_attitudes_by_wave.png", p2,
       width = 12, height = 7, dpi = 150)
cat("Salvato: Figure_2_attitudes_by_wave.png\n")
rm(w1, w2, w3, plot_data, p2)




#' ===================================================================
#' # 3. Quantile regression [G&A + ESTENSIONE wave 4]
#' ===================================================================

d1 <- dati[[1]]
d1$white  <- as.factor(d1$white)
d1$sex    <- as.factor(d1$sex)
d1$wave   <- as.factor(d1$wave)
d1$age_s  <- as.numeric(d1$age) / 10
d1$zrob   <- as.numeric(scale(d1$rob))

#' --- Wave 1-3 (replica G&A) ---
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
cat("Salvato: Figure_3a_quantreg_wave123.png\n")

#' --- Wave 1-4 (ESTENSIONE) ---
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
cat("Salvato: Figure_3b_quantreg_wave1234.png\n")
rm(d1, d1_123, fm_plot, fm_plot4)




#' ===================================================================
#' # 4. Traiettoria longitudinale Italia vs benchmark [ESTENSIONE]
#' ===================================================================
#' [FIX] Usa rob2item (2 item comparabili, scala 0-6) per tutte le wave.

cat("\n=== FIGURA 4: TRAIETTORIA LONGITUDINALE (rob2item) ===\n")

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
paese_types <- c(IT = "solid", DE = "dashed", FR = "dashed",
                 DK = "dotted", SE = "dotted", GR = "dashed",
                 PT = "dotted", ES = "dotted", EU27 = "solid")
paese_sizes <- c(IT = 1.5, DE = 0.8, FR = 0.8, DK = 0.8, SE = 0.8,
                 GR = 0.8, PT = 0.8, ES = 0.8, EU27 = 1.2)

traj_data$cntry <- factor(traj_data$cntry,
                          levels = c("IT","EU27","DE","FR","DK",
                                     "SE","GR","PT","ES"))

p4 <- ggplot(traj_data, aes(x = anno, y = mean,
                            colour = cntry, linetype = cntry,
                            linewidth = cntry)) +
  geom_line() +
  geom_point(size = 2) +
  scale_colour_manual(values = paese_colors, name = "Country") +
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
  labs(title = "Attitudes toward robots and AI: Italy vs. EU benchmark",
       subtitle = "Weighted composite score (rob2item, 2 comparable items, 0-6), 2012-2024",
       x = "Year", y = "Mean composite score (rob2item, 0-6)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank())

ggsave("./plots/Figure_4_italy_trajectory.png", p4,
       width = 11, height = 6, dpi = 150)
cat("Salvato: Figure_4_italy_trajectory.png\n")
rm(traj_data, eu_traj, p4, benchmark_countries)




#' ===================================================================
#' # 5. UAI vs composite score per paese [ESTENSIONE]
#' ===================================================================
#' [FIX] Usa rob2item per entrambi i panel (2017 e 2024).

cat("\n=== FIGURA 5: UAI vs COMPOSITE SCORE (rob2item) ===\n")

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

#' **Correlazioni per il testo della tesi**
r_2017 <- cor(scatter_data$UAI, scatter_data$score_w3, use = "complete.obs")
r_2024 <- cor(scatter_data$UAI, scatter_data$score_w4, use = "complete.obs")
cat(sprintf("  r(UAI, rob2item_2017) = %.3f\n", r_2017))
cat(sprintf("  r(UAI, rob2item_2024) = %.3f\n", r_2024))

scatter_long <- rbind(
  data.frame(cntry = scatter_data$cntry, UAI = scatter_data$UAI,
             score = scatter_data$score_w3, wave = "2017 (rob2item)",
             is_italy = scatter_data$is_italy),
  data.frame(cntry = scatter_data$cntry, UAI = scatter_data$UAI,
             score = scatter_data$score_w4, wave = "2024 (rob2item)",
             is_italy = scatter_data$is_italy)
)
scatter_long <- scatter_long[!is.na(scatter_long$score) &
                               !is.na(scatter_long$UAI), ]

#' **Etichette r per annotazione nei facet**
r_labels <- data.frame(
  wave = c("2017 (rob2item)", "2024 (rob2item)"),
  label = c(sprintf("r = %.3f", r_2017), sprintf("r = %.3f", r_2024)),
  UAI = 100, score = max(scatter_long$score, na.rm = TRUE) * 0.95
)

p5 <- ggplot(scatter_long,
             aes(x = UAI, y = score, colour = is_italy)) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey50",
              fill = "grey85", linewidth = 0.7, linetype = "dashed") +
  geom_point(aes(size = is_italy)) +
  geom_text_repel(aes(label = cntry),
                  size = 2.8, max.overlaps = 20,
                  segment.size = 0.2) +
  geom_text(data = r_labels, aes(x = UAI, y = score, label = label),
            inherit.aes = FALSE, hjust = 1, vjust = 1,
            size = 4, fontface = "italic", colour = "grey30") +
  scale_colour_manual(values = c("FALSE" = col_neutral, "TRUE" = col_italy),
                      labels = c("EU countries", "Italy"),
                      name = NULL) +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4), guide = "none") +
  facet_wrap(~ wave, scales = "free_y") +
  labs(title = "Uncertainty Avoidance Index vs. robot attitudes by country",
       subtitle = "Each dot = one EU country; dashed line = OLS regression with 95% CI",
       x = "Hofstede Uncertainty Avoidance Index (UAI)",
       y = "Weighted mean composite score (rob2item, 0-6)") +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(size = 13, face = "bold"),
        legend.position = "bottom")

ggsave("./plots/Figure_5_UAI_scatter.png", p5,
       width = 12, height = 6, dpi = 150)
cat("Salvato: Figure_5_UAI_scatter.png\n")
rm(scatter_data, scatter_long, r_labels, r_2017, r_2024, p5)




#' ===================================================================
#' # 6. Random effects per paese con highlight Italia [ESTENSIONE]
#' ===================================================================

cat("\n=== FIGURA 6: RANDOM EFFECTS PER PAESE ===\n")

#' Carica modelli (già stimati nello script 4)
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

#' Stima modelli A2 e B2 (wave 1-4) per estrarre random effects
library(lme4)
library(lmerTest)

m_A2 <- lapply(dati, function(x) {
  lmer(rob ~ wave + sex + age + educ + white +
         AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
         (1 | cid),
       data = x, weights = x$wgt2,
       control = lmerControl(optimizer = "nloptwrap"))
})

m_B2 <- lapply(dati, function(x) {
  lmer(rob ~ wave + sex + age + educ + white +
         AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG + UAI_z +
         (1 | cid),
       data = x, weights = x$wgt2,
       control = lmerControl(optimizer = "nloptwrap"))
})

#' Pool random effects (media across imputazioni)
re_A2 <- Reduce("+", lapply(m_A2, function(m) ranef(m)$cid)) / length(m_A2)
re_B2 <- Reduce("+", lapply(m_B2, function(m) ranef(m)$cid)) / length(m_B2)

re_A2$cid <- as.integer(rownames(re_A2))
re_B2$cid <- as.integer(rownames(re_B2))
cid_cntry <- unique(dati[[1]][, c("cid", "cntry")])
re_A2 <- merge(re_A2, cid_cntry, by = "cid")
re_B2 <- merge(re_B2, cid_cntry, by = "cid")
names(re_A2)[2] <- "re_A2"
names(re_B2)[2] <- "re_B2"

re_long <- rbind(
  data.frame(cntry = re_A2$cntry, re = re_A2$re_A2,
             modello = "A2 (without UAI)"),
  data.frame(cntry = re_B2$cntry, re = re_B2$re_B2,
             modello = "B2 (with UAI)")
)
re_long$is_italy <- re_long$cntry == "IT"

#' Ordina paesi per residuo A2
ord <- re_A2$cntry[order(re_A2$re_A2)]
re_long$cntry <- factor(re_long$cntry, levels = ord)

p6 <- ggplot(re_long, aes(x = cntry, y = re)) +
  geom_hline(yintercept = 0, colour = "grey30", linetype = "dashed",
             alpha = 0.5) +
  #' Linee verticali che collegano A2 e B2 per ogni paese
  #' (disegnate PRIMA dei punti così i punti restano in primo piano)
  geom_line(aes(group = cntry, colour = is_italy),
            linewidth = 0.6, alpha = 0.5) +
  geom_point(aes(colour = is_italy, shape = modello), size = 2.5) +
  scale_colour_manual(values = c("FALSE" = col_neutral, "TRUE" = col_italy),
                      labels = c("Other EU countries", "Italy"),
                      name = NULL) +
  scale_shape_manual(values = c("A2 (without UAI)" = 16,
                                "B2 (with UAI)"    = 17),
                     name = "Model") +
  annotate("text", x = which(levels(re_long$cntry) == "IT"),
           y = min(re_long$re) - 0.06,
           label = "Italy", colour = col_italy,
           size = 3.5, fontface = "bold") +
  annotate("text", x = length(levels(re_long$cntry)) - 1,
           y = max(re_long$re) + 0.04,
           label = "Above model\nprediction",
           colour = "#4575b4", size = 3, fontface = "italic",
           hjust = 0.5) +
  annotate("text", x = 2,
           y = min(re_long$re) + 0.04,
           label = "Below model\nprediction",
           colour = "#d73027", size = 3, fontface = "italic",
           hjust = 0.5) +
  labs(title = "Country random effects: Model A2 vs. Model B2 (wave 1-4)",
       subtitle = "Lines connect A2 → B2 for each country. Negative = more sceptical than predictors would expect",
       x = "Country", y = "Random intercept (residual)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        legend.position = "bottom",
        legend.box = "horizontal")

ggsave("./plots/Figure_6_random_effects.png", p6,
       width = 13, height = 6, dpi = 150)
cat("Salvato: Figure_6_random_effects.png\n")


cat("\n=== GRAFICI COMPLETATI ===\n")
cat("File salvati in ./plots/\n")
cat(list.files("./plots/"), sep = "\n")
cat("\n✓ Script 5 completato. Procedere con 6__Document_results_extended.R\n")
