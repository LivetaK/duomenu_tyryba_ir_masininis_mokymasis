# Auto-aprašomoji analizė visiems CSV "lab2" kataloge
if (!require("skimr")) install.packages("skimr", repos = "https://cloud.r-project.org/")
library(skimr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)

# ---- Helper funkcija: saugus folderių kūrimas ----
safe_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# ---- Baziniai folderiai rezultatams ----
safe_dir("2_lab_csv/aprasomojiStatistika")
safe_dir("t-SNE/2_lab_plots_aprasomoji_analize")
safe_dir("2_lab/2_lab_histograms_by_label")

# ---- Pagrindinė analizės funkcija ----
analyze_dataset <- function(input_path, dataset_name) {
  csv_out  <- file.path("2_lab_csv/aprasomojiStatistika", dataset_name)
  plots_out <- file.path("t-SNE/2_lab_plots_aprasomoji_analize", dataset_name)
  hist_out  <- file.path("2_lab/2_lab_histograms_by_label", dataset_name)

  safe_dir(csv_out); safe_dir(plots_out); safe_dir(hist_out)

  message("Analizuojamas duomenu rinkinys: ", dataset_name)

  # --- Nuskaitome duomenis
  df <- read.csv(input_path, sep = ",", header = TRUE, check.names = FALSE)

  df$label <- as.factor(df$label)

  # --- Tik kiekybiniai kintamieji (be klases (label)) ---
  num_vars <- df[, sapply(df, is.numeric) & names(df) != "label", drop = FALSE]

  # --- Aprasomoji statistika, bendra ---
  stats_list <- lapply(num_vars, function(x) {
    m <- mean(x, na.rm = TRUE)
    v <- var(x, na.rm = TRUE)
    standNuokrypis <- sqrt(v)
    varKoef <- (standNuokrypis / abs(m)) * 100

    var_class2 <- if(varKoef < 30) "maza" else if(varKoef < 70) "vidutine" else "didele"

    numeric_part <- round(c(
      Min. = min(x, na.rm = TRUE),
      Q1 = quantile(x, 0.25, na.rm = TRUE),
      Med. = median(x, na.rm = TRUE),
      Vid. = m,
      Q3 = quantile(x, 0.75, na.rm = TRUE),
      Maks. = max(x, na.rm = TRUE),
      Disp. = v,
      SdNuok. = standNuokrypis,
      VarK. = varKoef
    ), 4)

    c(numeric_part,
      VarKKlase = var_class2)
  })

  stats_df <- data.frame(Variable = names(stats_list), do.call(rbind, stats_list))
  write.csv(stats_df, file.path(csv_out, "aprasomoji_statistika_bendra.csv"), row.names = FALSE)

  # --- Aprasomoji statistika pagal klases (label) ---
  stats_by_label <- lapply(split(df, df$label), function(sub_df) {
    nv <- sub_df[, sapply(sub_df, is.numeric) & names(sub_df) != "label", drop = FALSE]
    sl <- lapply(nv, function(x) {
      m <- mean(x, na.rm = TRUE)
      v <- var(x, na.rm = TRUE)
      standNuokrypis <- sqrt(v)
      varKoef <- (standNuokrypis / abs(m)) * 100

      var_class2 <- if(varKoef < 30) "maza" else if(varKoef < 70) "vidutine" else "didele"

      numeric_part <- round(c(
        Min. = min(x, na.rm = TRUE),
        Q1 = quantile(x, 0.25, na.rm = TRUE),
        Med. = median(x, na.rm = TRUE),
        Vid. = m,
        Q3 = quantile(x, 0.75, na.rm = TRUE),
        Maks. = max(x, na.rm = TRUE),
        Disp. = v,
        SdNuok. = standNuokrypis,
        VarK. = varKoef
      ), 4)

      c(numeric_part,
        VarKKlase = var_class2)
    })
    data.frame(Label = as.character(unique(sub_df$label)),
               Variable = names(sl),
               do.call(rbind, sl),
               row.names = NULL, stringsAsFactors = FALSE)
  })

  stats_by_label_df <- do.call(rbind, stats_by_label)
  write.csv(stats_by_label_df, file.path(csv_out, "aprasomoji_statistika_by_label.csv"), row.names = FALSE)

  # --- Staciakampes diagramos ---
  for (var in names(num_vars)) {
    p <- ggplot(df, aes(x = label, y = .data[[var]], fill = label)) +
      geom_boxplot(outlier.color = "red", alpha = 0.6) +
      labs(title = paste(var, "staciakampe diagrama pagal klase"),
           x = "Klase (label)", y = var) +
      theme_minimal()
    safe_var <- gsub("[^[:alnum:]_]", "_", var)
    ggsave(file.path(plots_out, paste0("boxplot_", safe_var, ".png")), p,
           width = 7, height = 7, dpi = 300)
  }

  # --- Histogramos pagal dispersijos klase (priskiriame pagal Variacijos koeficientą) ---
  vars_to_plot <- stats_df$Variable[stats_df$Var_koef_klase == "didele"]
  if(length(vars_to_plot) == 0) vars_to_plot <- names(num_vars)

  for (lv in levels(df$label)) {
    df_subset <- df[df$label == lv, , drop = FALSE]
    for (var in vars_to_plot) {
      p <- ggplot(df_subset, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
        labs(title = paste(var, "histograma klasei", lv), x = var, y = "Daznis") +
        theme_minimal()
      safe_var <- gsub("[^[:alnum:]_]", "_", var)
      ggsave(file.path(hist_out, paste0("hist_", safe_var, "_label_", lv, ".png")), p,
             width = 7, height = 5, dpi = 300)
    }
  }

  # --- Apjungtos histogramos pagal klases ---
  for (var in vars_to_plot) {
    p <- ggplot(df, aes(x = .data[[var]], fill = label)) +
      geom_histogram(position = "identity", alpha = 0.5, bins = 30, color = "black") +
      labs(title = paste(var, "histograma pagal klases"), x = var, y = "Daznis", fill = "Klase") +
      theme_minimal()
    safe_var <- gsub("[^[:alnum:]_]", "_", var)
    ggsave(file.path(hist_out, paste0("hist_joined_", safe_var, ".png")), p,
           width = 7, height = 5, dpi = 300)
  }

  message("✅ Baigta: ", dataset_name)
}

# ---- Automatinis visu CSV apdorojimas ----
input_files <- list.files("2_lab_csv/lab2", pattern = "\\.csv$", full.names = TRUE)
input_files <- input_files[!grepl("normalized", input_files, ignore.case = TRUE)]

for (file in input_files) {
  dataset_name <- tools::file_path_sans_ext(basename(file))
  dataset_name <- gsub("[^[:alnum:]_]", "_", dataset_name)
  analyze_dataset(file, dataset_name)
}
