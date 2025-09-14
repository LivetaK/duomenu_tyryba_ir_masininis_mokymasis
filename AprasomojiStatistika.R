install.packages("skimr", repos = "https://cloud.r-project.org/") # Parsisiunčiame ir importuojame biblioteką darbui su duomenimis
library(skimr)

df <- read.csv("atrinkti_duomenys.csv", sep = ",", header = TRUE, check.names = FALSE) # Nuskaitome failą

cols_to_keep <- c("RR_l_0.RR_l_1", "RR_l_1.RR_l_2", "signal_mean", "signal_std", "R_val", "P_val", "label") # Parenkame kuriuos rodiklius norime pasilikti analizei

df_selected <- df[, cols_to_keep] # Sukuriame naują duomenų rinkinį su 7 pasirinktais rodikliais
write.csv(df_selected, "atrinkti_duomenys_6_rodikliai.csv", row.names = FALSE) # Išsaugome naują duomenų rinkinį į CSV failą

num_vars <- df_selected[, !names(df_selected) %in% "label"] # Analizei atrenkame tik kiekybinius rodikliu, neįtraukdami kokybinio rodiklio klasė (angl. label)

stats_list <- lapply(num_vars, function(x) { # Atliekame aprašomąją duomenų analizę su atrinktais rodikliais
  c(
    Minimumas = min(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Mediana = median(x, na.rm = TRUE),
    Vidurkis = mean(x, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    Maksimumas = max(x, na.rm = TRUE),
    Dispersija = var(x, na.rm = TRUE),
    `Neegzistuojancios reiksmes` = sum(is.na(x))
  )
})

stats_df <- data.frame( # Paverčiame statistikos sąrašą į duomenų rėmą ir pridedame kintamųjų pavadinimus, kad būtų galima teisingai eksportuoti į .CSV failą
  Variable = names(stats_list),
  do.call(rbind, stats_list)
)

write.csv(stats_df, "aprasomoji_atrinktu_duomenu_statistika.csv", row.names = FALSE) # Išsaugome aprašomosios duomenų analizės rezultatų rinkinį į .CSV failą
