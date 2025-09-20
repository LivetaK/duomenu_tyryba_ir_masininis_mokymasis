data <- read.csv('csv/duomenys_be_praleistu_reiksmiu_0918.csv', sep = ',')

data_0 <- data[data$label == 0, ]
data_1 <- data[data$label == 1, ]
data_2 <- data[data$label == 2, ]

write.csv(data_0, 'csv/duomenys_be_praleistu_reiksmiu_0918/label_0_0918.csv', row.names = FALSE)
write.csv(data_1, 'csv/duomenys_be_praleistu_reiksmiu_0918/label_1_0918.csv', row.names = FALSE)
write.csv(data_2, 'csv/duomenys_be_praleistu_reiksmiu_0918/label_2_0918.csv', row.names = FALSE)
