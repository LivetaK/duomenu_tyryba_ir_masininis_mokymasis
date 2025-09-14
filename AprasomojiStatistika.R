install.packages("skimr", repos = "https://cloud.r-project.org/")
library(skimr)

df <- read.csv("atrinkti_duomenys.csv", sep = ",", header = TRUE, check.names = FALSE)

cols_to_keep <- c("RR_l_0.RR_l_1", "RR_l_1.RR_l_2", "signal_mean", "signal_std", "R_val", "P_val", "label")

df_selected <- df[, cols_to_keep]

skim(df_selected)


