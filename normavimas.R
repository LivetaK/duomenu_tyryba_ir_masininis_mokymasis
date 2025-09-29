df <- read.csv("csv/final_clean_all_data.csv", stringsAsFactors = FALSE)

label_col <- "label"
pozymiai <- df[ , !(names(df) %in% label_col), drop = FALSE]

# Konvertuoti į numeric: faktorius/tekstus -> pakeisti kablelį į tašką ir as.numeric
pozymiai_num <- as.data.frame(lapply(pozymiai, function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}), stringsAsFactors = FALSE)

# Paliekam tik stulpelius, kuriuose yra bent šiek tiek ne-NA skaičių
pozymiai_num <- pozymiai_num[ , vapply(pozymiai_num, function(col) any(!is.na(col)), logical(1)), drop = FALSE]

# Saugus min–max (jei max==min -> grąžina 0)
min_max_norm <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (is.infinite(rng[1]) || is.infinite(rng[2]) || (rng[2] - rng[1]) == 0) {
    return(rep(0, length(x)))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}

# Standartizacija (jei sd=0, gausis NaN – pakeičiam 0)
vidurkis_sd <- as.data.frame(scale(pozymiai_num))
vidurkis_sd[!is.finite(as.matrix(vidurkis_sd))] <- 0
vidurkis_sd$label <- df[[label_col]]

# Min–max
min_max <- as.data.frame(lapply(pozymiai_num, min_max_norm))
min_max$label <- df[[label_col]]

write.csv(vidurkis_sd, "csv/normalized_mean_sd.csv", row.names = FALSE)
write.csv(min_max, "csv/normalized_min_max.csv", row.names = FALSE)
