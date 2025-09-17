data <- read.csv('csv/duomenys_be_praleistu_reiksmiu.csv', sep = ',')

data_0 <- data[data$label == 0, ]
data_1 <- data[data$label == 1, ]
data_2 <- data[data$label == 2, ]

write.csv(data_0, 'csv/duomenys_be_praleistu_reiksmiu/label_0.csv', row.names = FALSE)
write.csv(data_1, 'csv/duomenys_be_praleistu_reiksmiu/label_1.csv', row.names = FALSE)
write.csv(data_2, 'csv/duomenys_be_praleistu_reiksmiu/label_2.csv', row.names = FALSE)
