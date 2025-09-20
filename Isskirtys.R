# # funkcija skaiciuojanti isskirtis
# outliers <- function(data, column) {
# boxplot.stats(data[[column]])$out
# }
# #funkcija salinanti isskirtis
# out_delete <- function(data, column, outlier) {
# data_clean <- data[!data[[column]] %in% outlier, ]
# return(data_clean)
# }
# tiriamasis <- 'RR_r_0.RR_r_1'
columns <- c("signal_std",
                  "wl_side",
                  "R_val",
                  "RR_r_0.RR_r_1",
                  "P_val",
                  "Q_val",
                  "label")


label_0 <- read.csv("csv/duomenys_be_praleistu_reiksmiu_0918/label_1_0918.csv", sep = ',')
# funkcija nustatanti ekstremalias isskirtis
extreme_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
#jeigu nori pamatyt mild, pakeiski 1.5 is 3
  lower_extreme <- Q1 - 3 * IQR
  upper_extreme <- Q3 + 3 * IQR

  out <- data[[column]][data[[column]] < lower_extreme | data[[column]] > upper_extreme]
  return(out)
}

for (col in columns){
  tiriamasis <- col
  outlier <- extreme_outliers(label_0, tiriamasis)
  extreme_kiekis <- length(outlier)
  print(col, "_extreme_kiekis: ", extreme_kiekis )
}

# # boxplot su pažymėtomis ekstremaliomis išskirtimis
# boxplot(label_0[[tiriamasis]], main = paste(tiriamasis, "su ekstremaliomis"))
# points(which(label_0[[tiriamasis]] %in% outlier), outlier, col = "red", pch = 19)
