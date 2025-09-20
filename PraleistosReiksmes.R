raw_data<- read.csv("csv/atrinkti_duomenys_6_rodikliai.csv", sep = ",")

# praleistų reikšmių nustatymas
pr_reiksmes <- colSums(is.na(raw_data))

# praleistų reikšmių šalinimas
data <- raw_data[!is.na(raw_data$R_val), ]

write.csv(data, "csv/duomenys_be_praleistu_reiksmiu_0918_seed67.csv", row.names = FALSE)