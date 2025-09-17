# funkcija skaiciuojanti isskirtis
outliers <- function(data, column) {
boxplot.stats(data[[column]])$out
}
#funkcija salinanti isskirtis
out_delete <- function(data, column, outlier) {
data_clean <- data[!data[[column]] %in% outlier, ]
return(data_clean)
}

label_0 <- read.csv("csv/duomenys_be_praleistu_reiksmiu/label_0.csv", sep = ',')
boxplot(label_0)
