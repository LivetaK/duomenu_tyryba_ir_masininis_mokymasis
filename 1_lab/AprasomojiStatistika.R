if (!require("skimr")) install.packages("skimr", repos = "https://cloud.r-project.org/") # Parsisiunčiame ir importuojame biblioteką darbui su duomenimis
library(skimr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)

if(!dir.exists("csv/aprasomojiStatistika")) dir.create("csv/aprasomojiStatistika")

# --- Nuskaitome duomenis

df <- read.csv("csv/raw_data_1500_6.csv", sep = ",", header = TRUE, check.names = FALSE) # Nuskaitome failą
df_selected <- df # Sukuriame df_selected kaip df kopiją, kad būtų galima naudoti tolesnėje analizėje
# --- Aprašomoji duomenų analizė (bendra)

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

write.csv(stats_df, "csv/aprasomojiStatistika/aprasomoji_atrinktu_duomenu_statistika.csv", row.names = FALSE) # Išsaugome aprašomosios duomenų analizės rezultatų rinkinį į .CSV failą

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

write.csv(stats_by_label_df, "csv/aprasomojiStatistika/aprasomoji_statistika_by_label.csv", row.names = FALSE)

# --- Stačiakampės diagramos (boxplot) pagal klases (label)
num_vars <- setdiff(names(df_selected), "label") # Išrenkame tik kiekybinius kintamuosius, neįtraukdami kokybinio kintamojo klasė (angl. label)

if (!dir.exists("plots_aprasomoji_analize")) dir.create("plots_aprasomoji_analize")

for (var in num_vars) {
  p <- ggplot(df_selected, aes(x = factor(label, levels = c(0,1,2),
  labels = c("Klasė 0", "Klasė 1", "Klasė 2")),
  y = .data[[var]], fill = factor(label))) +
    geom_boxplot(outlier.color = "red", alpha = 0.6) +
    labs(title = paste(var, "stačiakampė diagrama pagal klasę"),
         x = "Klasė (label)",
         y = var) +
    theme_minimal()

  # Sutvarkome kintamuosius
  safe_var <- gsub("[^[:alnum:]_]", "_", var)

  ggsave(
    filename = paste0("plots_aprasomoji_analize/boxplot_", safe_var, ".png"),
    plot = p,
    width = 7, height = 7, dpi = 300
  )
}

# --- Histogramos pagal klases (label)

vars_to_plot <- c("wl_side", "Q_val") # Požymiai, kurių histogramos bus braižomos

# Požymių reikšmių histogramos pagal klasę (label)
label_values <- c(0, 1, 2)

if (!dir.exists("histograms_by_label")) dir.create("histograms_by_label")

# Iteruojame per kiekvieną klasę (label)
for (label_value in label_values) {
  df_subset <- df_selected[df_selected$label == label_value, ]

  for (var in vars_to_plot) {
    p <- ggplot(df_subset, aes(x = .data[[var]])) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
      labs(
        title = paste(var, "pozymio histograma", "klasei", label_value),
        x = var,
        y = "Daznis"
      ) +
      theme_minimal()

    safe_var <- gsub("[^[:alnum:]_]", "_", var)
    ggsave(
      filename = paste0("histograms_by_label/hist_", safe_var, "_label_", label_value, ".png"),
      plot = p,
      width = 7, height = 5, dpi = 300
    )
  }
}

vars_to_plot <- c("wl_side", "Q_val")  # Parenkame požymius

for (var in vars_to_plot) {
  p <- ggplot(df_selected, aes(x = .data[[var]], fill = factor(label))) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30, color = "black") +
    labs(
      title = paste(var, "pozymio histograma", "pagal klases"),
      x = var,
      y = "Daznis",
      fill = "Klase"
    ) +
    theme_minimal()

  safe_var <- gsub("[^[:alnum:]_]", "_", var)  # Sutvarkome kintamuosius
    ggsave(
      filename = paste0("histograms_by_label/hist_joined", safe_var,".png"),
      plot = p,
      width = 7, height = 5, dpi = 300
    )
}