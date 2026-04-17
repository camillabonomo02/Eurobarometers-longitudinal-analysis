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
#'   Figure 2   — Barplot: medie per wave e tipo di applicazione (replica + estensione)
#'   Figure 3   — Quantile regression: predittori individuali (replica + wave 4)
#'   Figure 4   — Traiettoria longitudinale: Italia vs benchmark EU (ESTENSIONE)
#'   Figure 5   — Scatter: UAI vs composite score per paese (ESTENSIONE)
#'   Figure 6   — Random effects per paese con highlight Italia (ESTENSIONE)


#' **Clear workspace**
rm(list = ls())

#' **Load packages**
library(doBy)
library(ggplot2)
library(ggrepel)    # etichette non sovrapposte nei scatter
library(mitml)
library(weights)
library(grid)
library(quantreg)
library(dplyr)
# install.packages(c("ggrepel")) se mancante

source("./syntax/0_Start.R")

#' **Load data**
load("./data/dat.Rdata")
rm(dati_mice)

#' **Aggiungi rob2item e recode onde**
dati <- lapply(dati, function(x) {
  x$rob2item <- x$rob1 + x$rob2
  x
})
dati <- as.mitml.list(dati)

dir.create("./plots", showWarnings = FALSE)




#' ===================================================================
#' # 1. Mappe EU [G&A + ESTENSIONE wave 4]
#' ===================================================================
#' Usa rworldmap come G&A. Se non disponibile, sostituire con
#' approccio alternativo (vedere nota sotto).

maps_available <- requireNamespace("rworldmap", quietly = TRUE)

if (maps_available) {
  library(rworldmap)
  
  #' **Standardizza composite score rispetto a wave 3** [G&A]
  #' [FIX] describe.imp con 1 solo item crasha — uso weighted.mean diretto
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
    # Score standardizzato wave 3 [FIX: weighted.mean diretto]
    ds$rob_w3[ds$cntry == i] <- mean(sapply(dati, function(x) {
      sub <- x[x$cntry == i & x$wave_n == 3, ]
      if (nrow(sub) > 0) weighted.mean(sub$zrob, sub$wgt2, na.rm = TRUE) else NA
    }))
    
    # Score standardizzato wave 4 (non standardizzato — estensione)
    ds$rob_w4[ds$cntry == i] <- {
      sub <- dat[dat$cntry == i & dat$wave == 4 &
                   !is.na(dat$rob) & !is.na(dat$wgt2), ]
      if (nrow(sub) > 0) weighted.mean(sub$rob, sub$wgt2) else NA
    }
    
    # Cohen's d cambiamento wave 1 → 3 [G&A]
    if (i != "HR") {  # HR assente in wave 1
      ds$delta_13[ds$cntry == i] <-
        ttest.imp(rob ~ wave_n, dati, weights = "wgt2",
                  paired = FALSE, print = FALSE,
                  subset = (dati[[1]]$cntry == i &
                              dati[[1]]$wave_n %in% c(1, 3)))$d * -1
    }
    
    # Cohen's d cambiamento wave 1 → 4 [ESTENSIONE]
    if (i != "HR") {
      ds$delta_14[ds$cntry == i] <-
        ttest.imp(rob ~ wave_n, dati, weights = "wgt2",
                  paired = FALSE, print = FALSE,
                  subset = (dati[[1]]$cntry == i &
                              dati[[1]]$wave_n %in% c(1, 4)))$d * -1
    }
  }
  
  ds$rob_w3[ds$rob_w3 < -.50] <- -.495  # Greece floor (G&A)
  
  #' **Paesi EU senza UK** [ESTENSIONE]
  europeanUnion <- c("Austria","Belgium","Bulgaria","Cyprus",
                     "Czech Rep.","Denmark","Estonia","Finland","France",
                     "Germany","Greece","Croatia","Hungary","Ireland",
                     "Italy","Latvia","Lithuania","Luxembourg","Malta",
                     "Netherlands","Poland","Portugal","Romania",
                     "Slovakia","Slovenia","Spain","Sweden")
  
  sPDF <- joinCountryData2Map(ds, joinCode = "ISO2",
                              nameJoinColumn = "cntry")
  sPDFmyCountries <- sPDF[sPDF$NAME %in% europeanUnion, ]
  
  #' Palette colori per le mappe
  #' 1a/1c score: blu (scettico) → bianco → rosso (positivo)
  pal_score    <- colorRampPalette(c("#4575b4", "#abd9e9", "#f7f7f7",
                                     "#fdae61", "#d73027"))(12)
  #' 1b cambiamento: rosso = calo (negativo), bianco = stabile, blu = aumento
  pal_change   <- colorRampPalette(c("#d73027", "#f7f7f7", "#4575b4"))(12)
  #' 1c score 2024: sequenziale giallo → arancio → rosso scuro
  pal_score_w4 <- colorRampPalette(c("#ffffb2", "#fecc5c", "#fd8d3c",
                                     "#f03b20", "#bd0026"))(12)
  
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
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2))
  par(op); dev.off()
  cat("Salvato: Figure_1a_map_wave3.png\n")
  
  #' --- Figure 1b: Cambiamento wave 1 → 3 [G&A + scala corretta] ---
  #' Scala ristretta da -0.5 a 0.1 (i dati non superano questi estremi)
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
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2))
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
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2))
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
#' # 2. Barplot medie per wave [G&A + ESTENSIONE wave 4]
#' ===================================================================
#' Estende Figure 2 di G&A aggiungendo la wave 4 (rob e rob2item).
#' feel1-4 non disponibili in wave 4 — inclusi solo per wave 1-3.

cat("\n=== FIGURA 2: MEDIE PER WAVE ===\n")

#' **Raccogli medie per wave**
get_wave_means <- function(wave_num, items, dati) {
  describe.imp(dati, items = items, weights = "wgt2",
               stats = c("mean", "sd"), digits = 10,
               subset = (dati[[1]]$wave_n == wave_num))
}

# Recode wave_n se non ancora presente
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
# [FIX] w4 ha un solo item — describe.imp crasha, uso weighted.mean diretto
w4_mean <- mean(sapply(dati, function(x) {
  sub <- x[x$wave_n == 4, ]
  weighted.mean(sub$rob, sub$wgt2, na.rm = TRUE)
}))
w4_sd <- mean(sapply(dati, function(x) {
  sub <- x[x$wave_n == 4, ]
  sqrt(wtd.var(sub$rob, sub$wgt2))
}))
w4 <- matrix(c(1, sum(dati[[1]]$wave_n == 4), w4_mean, w4_sd),
             nrow = 1,
             dimnames = list("rob", c("vars", "n", "mean", "sd")))

#' **Dataset per ggplot2** (formato long)
plot_data <- data.frame(
  wave  = rep(c("2012", "2014", "2017", "2024"), each = 5),
  item  = rep(c("General\nappraisal", "Medical\noperation",
                "Assisting\nat work", "Services\nfor elderly",
                "Driverless\ncars"), 4),
  mean  = c(
    w1["rob","mean"],  w1["feel1","mean"], w1["feel2","mean"], NA, NA,
    w2["rob","mean"],  w2["feel1","mean"], w2["feel2","mean"],
    w2["feel3","mean"], w2["feel4","mean"],
    w3["rob","mean"],  w3["feel1","mean"], w3["feel2","mean"],
    w3["feel3","mean"], w3["feel4","mean"],
    w4["rob","mean"],  NA, NA, NA, NA
  ),
  sd = c(
    w1["rob","sd"],    w1["feel1","sd"],   w1["feel2","sd"],   NA, NA,
    w2["rob","sd"],    w2["feel1","sd"],   w2["feel2","sd"],
    w2["feel3","sd"],   w2["feel4","sd"],
    w3["rob","sd"],    w3["feel1","sd"],   w3["feel2","sd"],
    w3["feel3","sd"],   w3["feel4","sd"],
    w4["rob","sd"],    NA, NA, NA, NA
  )
)
plot_data$wave <- factor(plot_data$wave,
                         levels = c("2012","2014","2017","2024"))
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
  scale_fill_manual(values = c("2012" = "#2166ac", "2014" = "#74add1",
                               "2017" = "#fdae61", "2024" = "#d73027"),
                    name = "Wave") +
  scale_y_continuous(limits = c(0, 9), breaks = 0:9) +
  labs(title = "Mean attitudes towards robots and AI in Europe",
       subtitle = "Weighted means across EU27 (2012-2024)",
       x = NULL, y = "Mean attitude score (0-9)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank())

ggsave("./plots/Figure_2_attitudes_by_wave.png", p2,
       width = 12, height = 7, dpi = 150)
cat("Salvato: Figure_2_attitudes_by_wave.png\n")
rm(w1, w2, w3, w4, plot_data)




#' ===================================================================
#' # 3. Quantile regression [G&A + ESTENSIONE wave 4]
#' ===================================================================

#' --- Quantile regression: usa primo dataset imputato ---
#' [NOTA] rq non supporta pooling nativo su imputazioni multiple.
#' G&A usano il primo dataset (fm[[1]]) per il plot — stessa scelta qui.

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
#' Composite score ponderato per wave: IT, DE, FR, DK, SE, GR, PT, ES

cat("\n=== FIGURA 4: TRAIETTORIA LONGITUDINALE ITALIA vs BENCHMARK ===\n")

benchmark_countries <- c("IT", "DE", "FR", "DK", "SE", "GR", "PT", "ES")
anni <- c(2012, 2014, 2017, 2024)

traj_data <- do.call(rbind, lapply(benchmark_countries, function(cc) {
  do.call(rbind, lapply(1:4, function(w) {
    sub <- dat[dat$cntry == cc & dat$wave == w &
                 !is.na(dat$rob) & !is.na(dat$wgt2), ]
    if (nrow(sub) == 0) return(NULL)
    data.frame(
      cntry = cc,
      anno  = anni[w],
      wave  = w,
      mean  = weighted.mean(sub$rob, sub$wgt2),
      n     = nrow(sub)
    )
  }))
}))

# EU media
eu_traj <- do.call(rbind, lapply(1:4, function(w) {
  sub <- dat[dat$wave == w & !is.na(dat$rob) & !is.na(dat$wgt2), ]
  data.frame(cntry = "EU27", anno = anni[w], wave = w,
             mean = weighted.mean(sub$rob, sub$wgt2), n = nrow(sub))
}))
traj_data <- rbind(traj_data, eu_traj)

#' Colori e linetype
paese_colors <- c(
  IT = "#e41a1c", DE = "#377eb8", FR = "#4daf4a",
  DK = "#984ea3", SE = "#ff7f00", GR = "#a65628",
  PT = "#f781bf", ES = "#999999", EU27 = "black"
)
paese_types <- c(
  IT = "solid", DE = "dashed", FR = "dashed",
  DK = "dotted", SE = "dotted", GR = "dashed",
  PT = "dotted", ES = "dotted", EU27 = "solid"
)
paese_sizes <- c(
  IT = 1.5, DE = 0.8, FR = 0.8, DK = 0.8, SE = 0.8,
  GR = 0.8, PT = 0.8, ES = 0.8, EU27 = 1.2
)

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
  scale_y_continuous(limits = c(4, 8), breaks = seq(4, 8, 0.5)) +
  geom_text(data = subset(traj_data, anno == 2024 & cntry == "IT"),
            aes(label = "Italy"), nudge_x = 0.5, nudge_y = 0.01,
            colour = "#e41a1c", size = 3.5, fontface = "bold") +
  geom_text(data = subset(traj_data, anno == 2024 & cntry == "EU27"),
            aes(label = "EU27"), nudge_x = 0.5,
            colour = "black", size = 3.5) +
  labs(title = "Attitudes toward robots and AI: Italy vs. EU benchmark",
       subtitle = "Weighted composite score (0-9), 2012-2024",
       x = "Year", y = "Mean composite score (rob)") +
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
#' Scatter: UAI paese (asse x) vs score medio wave 3 e wave 4 (asse y)
#' con retta di regressione e highlight Italia

cat("\n=== FIGURA 5: UAI vs COMPOSITE SCORE ===\n")

#' Calcola medie per paese per wave 3 e wave 4
scatter_data <- do.call(rbind, lapply(unique(dat$cntry), function(cc) {
  uai_val <- dat$UAI[dat$cntry == cc][1]
  m3 <- {
    sub <- dat[dat$cntry == cc & dat$wave == 3 &
                 !is.na(dat$rob) & !is.na(dat$wgt2), ]
    if (nrow(sub) > 0) weighted.mean(sub$rob, sub$wgt2) else NA
  }
  m4 <- {
    sub <- dat[dat$cntry == cc & dat$wave == 4 &
                 !is.na(dat$rob) & !is.na(dat$wgt2), ]
    if (nrow(sub) > 0) weighted.mean(sub$rob, sub$wgt2) else NA
  }
  data.frame(cntry = cc, UAI = uai_val, score_w3 = m3, score_w4 = m4)
}))
scatter_data$is_italy <- scatter_data$cntry == "IT"

#' Plot wave 3 (replica logica G&A) + wave 4 affiancate
scatter_long <- rbind(
  data.frame(cntry = scatter_data$cntry,
             UAI   = scatter_data$UAI,
             score = scatter_data$score_w3,
             wave  = "2017",
             is_italy = scatter_data$is_italy),
  data.frame(cntry = scatter_data$cntry,
             UAI   = scatter_data$UAI,
             score = scatter_data$score_w4,
             wave  = "2024",
             is_italy = scatter_data$is_italy)
)
scatter_long <- scatter_long[!is.na(scatter_long$score) &
                               !is.na(scatter_long$UAI), ]

p5 <- ggplot(scatter_long,
             aes(x = UAI, y = score, colour = is_italy)) +
  geom_point(aes(size = is_italy)) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey40",
              linewidth = 0.7, linetype = "dashed") +
  geom_text_repel(aes(label = cntry),
                  size = 2.8, max.overlaps = 20,
                  segment.size = 0.2) +
  scale_colour_manual(values = c("FALSE" = "grey50", "TRUE" = "#e41a1c"),
                      guide = "none") +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4), guide = "none") +
  facet_wrap(~ wave, scales = "free_y") +
  labs(title = "Uncertainty Avoidance Index vs. robot attitudes by country",
       subtitle = "Each dot = one EU country; red = Italy; line = OLS regression",
       x = "Hofstede Uncertainty Avoidance Index (UAI)",
       y = "Weighted mean composite score") +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(size = 13, face = "bold"))

ggsave("./plots/Figure_5_UAI_scatter.png", p5,
       width = 12, height = 6, dpi = 150)
cat("Salvato: Figure_5_UAI_scatter.png\n")
rm(scatter_data, scatter_long, p5)




#' ===================================================================
#' # 6. Random effects per paese con highlight Italia [ESTENSIONE]
#' ===================================================================
#' Visualizza i residui paese dal Modello A2 (wave 1-4),
#' ordinati dal piu' basso al piu' alto, con Italia evidenziata.
#' Usa il primo dataset imputato (come nella sezione 6 dello script 4).

cat("\n=== FIGURA 6: RANDOM EFFECTS PAESE ===\n")

library(lme4)

d1 <- dati[[1]]
d1$wave    <- as.factor(d1$wave)
d1$white   <- as.factor(d1$white)
d1$sex     <- as.factor(d1$sex)
d1$age_s   <- as.numeric(d1$age) / 10
uai_mean   <- mean(unique(d1[, c("cid","UAI")])$UAI, na.rm = TRUE)
uai_sd     <- sd(unique(d1[, c("cid","UAI")])$UAI,   na.rm = TRUE)
d1$UAI_z   <- (d1$UAI - uai_mean) / uai_sd
d1$educ_s  <- as.numeric(scale(d1$educ, scale = FALSE))
d1$wgt2_n  <- d1$wgt2 / mean(d1$wgt2, na.rm = TRUE)

m_A2 <- lmer(rob ~ wave + sex + age_s + educ_s + white +
               AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG +
               (1 | cid),
             data    = d1,
             weights = wgt2_n,
             REML    = FALSE,
             control = lmerControl(optimizer = "nloptwrap"))

m_B2 <- lmer(rob ~ wave + sex + age_s + educ_s + white +
               AGEOLD + TECHEXP + INVEST + UNEMP + LAT + LONG + UAI_z +
               (1 | cid),
             data    = d1,
             weights = wgt2_n,
             REML    = FALSE,
             control = lmerControl(optimizer = "nloptwrap"))

re_A2 <- ranef(m_A2)$cid
re_B2 <- ranef(m_B2)$cid
re_A2$cid <- as.integer(rownames(re_A2))
re_B2$cid <- as.integer(rownames(re_B2))
cid_cntry <- unique(d1[, c("cid", "cntry")])
re_A2 <- merge(re_A2, cid_cntry, by = "cid")
re_B2 <- merge(re_B2, cid_cntry, by = "cid")
names(re_A2)[2] <- "re_A2"
names(re_B2)[2] <- "re_B2"

re_df <- merge(re_A2[, c("cntry","re_A2")],
               re_B2[, c("cntry","re_B2")], by = "cntry")
re_df$is_italy <- re_df$cntry == "IT"
re_df <- re_df[order(re_df$re_A2), ]
re_df$cntry <- factor(re_df$cntry, levels = re_df$cntry)

#' Formato long per ggplot
re_long <- rbind(
  data.frame(cntry = re_df$cntry, re = re_df$re_A2,
             modello = "A2 (without UAI)", is_italy = re_df$is_italy),
  data.frame(cntry = re_df$cntry, re = re_df$re_B2,
             modello = "B2 (with UAI)", is_italy = re_df$is_italy)
)

p6 <- ggplot(re_long, aes(x = cntry, y = re, group = modello)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  # linee separate per evitare conflitto colour + linetype variabili
  geom_line(data = subset(re_long, modello == "A2 (without UAI)"),
            colour = "grey50", linetype = "solid", alpha = 0.5) +
  geom_line(data = subset(re_long, modello == "B2 (with UAI)"),
            colour = "grey30", linetype = "dashed", alpha = 0.5) +
  geom_point(aes(colour = is_italy, shape = modello), size = 2.5) +
  scale_colour_manual(values = c("FALSE" = "grey60", "TRUE" = "#e41a1c"),
                      guide = "none") +
  scale_shape_manual(values = c("A2 (without UAI)" = 16,
                                "B2 (with UAI)"   = 17),
                     name = "Model") +
  annotate("text", x = which(levels(re_long$cntry) == "IT"),
           y = min(re_long$re) - 0.06,
           label = "Italy", colour = "#e41a1c",
           size = 3.5, fontface = "bold") +
  labs(title = "Country random effects: Model A2 vs. Model B2 (wave 1-4)",
       subtitle = "Negative values = below EU average after controlling for structural predictors",
       x = "Country", y = "Random intercept (residual)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        legend.position = "bottom")

ggsave("./plots/Figure_6_random_effects.png", p6,
       width = 13, height = 6, dpi = 150)

cat("\n=== GRAFICI COMPLETATI ===\n")
cat("File salvati in ./plots/\n")
cat(list.files("./plots/"), sep = "\n")
cat("\n✓ Script 5 completato. Procedere con 6__Document_results_extended.R\n")