
df <- read.csv("csv/clean_data.csv")

label_col <- "label"

# Požymių ir label'ių atskyrimui
pozymiai <- df[ , !(names(df) %in% label_col)]
labels <- df[[label_col]]

# Pagal vidurkį ir dispersiją
# (x - vidurkis) / sd

vidurkis_sd <- as.data.frame(scale(pozymiai))
vidurkis_sd$label <- labels  # Grąžiname label'į

# min-max normavimas

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
min_max <- as.data.frame(lapply(pozymiai, min_max_norm))
min_max$label <- labels


# Išsaugoti normalizuotas versijas
write.csv(vidurkis_sd, "csv/normalized_mean_sd.csv", row.names = FALSE)
write.csv(min_max, "csv/normalized_min_max.csv", row.names = FALSE)
