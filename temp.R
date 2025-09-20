df <- read.csv("csv/atrinkti_duomenys.csv", sep = ",", header = TRUE, check.names = FALSE) # Nuskaitome failą

# --- Aprašomoji duomenų analizė (bendra)

cols_to_keep <- c("signal_std",
                  "wl_side",
                  "R_val",
                  "RR_r_0.RR_r_1",
                  "P_val",
                  "Q_val",
                  "label") # Parenkame kuriuos rodiklius norime pasilikti analizei

df_selected <- df[, cols_to_keep] # Sukuriame naują duomenų rinkinį su 7 pasirinktais rodikliais
write.csv(df_selected, "csv/atrinkti_duomenys_6_rodikliai_0918.csv", row.names = FALSE) # Išsaugome naują duomenų rinkinį į CSV failą
