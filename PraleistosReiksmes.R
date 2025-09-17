raw_data<- read.csv("csv/atrinkti_duomenys_6_rodikliai.csv", sep = ",")

# praleistų reikšmių nustatymas
pr_reiksmes <- mapply(anyNA, raw_data)

# praleistų reikšmių šalinimas
data <- raw_data[!is.na(raw_data$R_val), ]

write.csv(data, "csv/duomenys_be_praleistu_reiksmiu.csv", row.names = FALSE)