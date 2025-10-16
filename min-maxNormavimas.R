
df <- read.csv("lab2/NO_missing_values_1498_6.csv", sep = ",", stringsAsFactors = FALSE)


label_col <- "label"


pozymiai <- df[ , !(names(df) %in% label_col), drop = FALSE]


pozymiai_num <- as.data.frame(lapply(pozymiai, function(x) {
  x <- gsub(",", ".", as.character(x))
  x <- trimws(x)
  suppressWarnings(as.numeric(x))
}), stringsAsFactors = FALSE)


pozymiai_num <- pozymiai_num[ , vapply(pozymiai_num, function(col) any(!is.na(col)), logical(1)), drop = FALSE]


min_max_norm <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (is.infinite(rng[1]) || is.infinite(rng[2]) || (rng[2] - rng[1]) == 0) {
    return(rep(0, length(x)))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}


min_max <- as.data.frame(lapply(pozymiai_num, min_max_norm))


min_max$label <- df[[label_col]]


write.csv(min_max, "lab2/normalized_NO_missing_1498_6.csv", row.names = FALSE)
