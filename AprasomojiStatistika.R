if (!require("skimr")) install.packages("skimr", repos = "https://cloud.r-project.org/") # Parsisiunčiame ir importuojame biblioteką darbui su duomenimis
library(skimr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)

if(!dir.exists("csv")) dir.create("csv")

# --- Nuskaitome duomenis

df <- read.csv("csv/atrinkti_duomenys.csv", sep = ",", header = TRUE, check.names = FALSE) # Nuskaitome failą

# --- Aprašomoji duomenų analizė (bendra)

#cols_to_keep <- c("RR_l_0.RR_l_1", "RR_l_1.RR_l_2", "signal_mean", "signal_std", "R_val", "P_val", "label") # Pirminiai rodikliai,
                                                                                                              #kuriuos atrinkome diskusijos ir medicininio reikšmingumo būdu

cols_to_keep <- c("signal_std", "wl_side", "R_val", "P_val", "RR_r_0.RR_r_1", "Q_val", "label") # Parenkame kuriuos rodiklius norime pasilikti analizei

df_selected <- df[, cols_to_keep] # Sukuriame naują duomenų rinkinį su 7 pasirinktais rodikliais
write.csv(df_selected, "csv/atrinkti_duomenys_6_rodikliai.csv", row.names = FALSE) # Išsaugome naują duomenų rinkinį į CSV failą

num_vars <- df_selected[, !names(df_selected) %in% "label"] # Analizei atrenkame tik kiekybinius rodikliu, neįtraukdami kokybinio rodiklio klasė (angl. label)

stats_list <- lapply(num_vars, function(x) { # Atliekame aprašomąją duomenų analizę su atrinktais rodikliais
  round(c(
    Minimumas = min(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Mediana = median(x, na.rm = TRUE),
    Vidurkis = mean(x, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    Maksimumas = max(x, na.rm = TRUE),
    Dispersija = var(x, na.rm = TRUE),
    `Neegzistuojancios reiksmes` = sum(is.na(x))
  ), 4)
})

stats_df <- data.frame( # Paverčiame statistikos sąrašą į duomenų rėmą ir pridedame kintamųjų pavadinimus, kad būtų galima teisingai eksportuoti į .CSV failą
  Variable = names(stats_list),
  do.call(rbind, stats_list)
)

write.csv(stats_df, "csv/aprasomoji_atrinktu_duomenu_statistika.csv", row.names = FALSE) # Išsaugome aprašomosios duomenų analizės rezultatų rinkinį į .CSV failą

if (!dir.exists("pics_aprasomoji_analize")) dir.create("pics_aprasomoji_analize")

table_grob <- tableGrob(stats_df, rows = NULL) # Sukuriame lentelę iš gautų rezultatų excel failo
png("pics_aprasomoji_analize/aprasomoji_statistika_bendras_vaizdas.png", width = 800, height = 180)
grid.draw(table_grob)
dev.off()

# --- Aprašomoji duomenų analizė pagal klases (label)
stats_by_label <- lapply(split(df_selected, df_selected$label), function(sub_df) {
  num_vars <- sub_df[, !names(sub_df) %in% "label"]
  stats_list <- lapply(num_vars, function(x) {
    round(c(
      Minimumas = min(x, na.rm = TRUE),
      Q1 = quantile(x, 0.25, na.rm = TRUE),
      Mediana = median(x, na.rm = TRUE),
      Vidurkis = mean(x, na.rm = TRUE),
      Q3 = quantile(x, 0.75, na.rm = TRUE),
      Maksimumas = max(x, na.rm = TRUE),
      Dispersija = var(x, na.rm = TRUE),
      Neegzistuojancios.reiksmes = sum(is.na(x))
    ), 4)
  })

  df_stats <- data.frame(
    Label = unique(sub_df$label),
    Variable = names(stats_list),
    do.call(rbind, stats_list)
  )
  return(df_stats)
})

stats_by_label_df <- do.call(rbind, stats_by_label)

write.csv(stats_by_label_df, "csv/aprasomoji_statistika_by_label.csv", row.names = FALSE)

for(lbl in unique(stats_by_label_df$Label)) {
  df_label <- stats_by_label_df[stats_by_label_df$Label == lbl, ]
  table_grob <- tableGrob(df_label, rows = NULL)
  png(paste0("pics_aprasomoji_analize/aprasomoji_statistika_label_", lbl, ".png"), width = 900, height = 180)
  grid.draw(table_grob)
  dev.off()
}

# --- Stačiakampės diagramos (boxplot) pagal klases (label)
num_vars <- setdiff(names(df_selected), "label") # Išrenkame tik kiekybinius kintamuosius, neįtraukdami kokybinio kintamojo klasė (angl. label)

if (!dir.exists("plots_aprasomoji_analize")) dir.create("plots_aprasomoji_analize")

for (var in num_vars) {
  p <- ggplot(df_selected, aes(x = factor(label, levels = c(0,1,2), labels = c("Klasė 0", "Klasė 1", "Klasė 2")),
                               y = .data[[var]], fill = factor(label))) +
    geom_boxplot(outlier.color = "red", alpha = 0.6) +
    labs(title = paste(var, "stačiakampė diagrama pagal klasę"),
         x = "Klasė (label)",
         y = var) +
    theme_minimal()

  ggsave(
    filename = paste0("plots_aprasomoji_analize/boxplot_", var, ".png"),
    plot = p,
    width = 7, height = 7, dpi = 300
  )
}