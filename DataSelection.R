library(dplyr)

if(!dir.exists("csv")) dir.create("csv")
df <- read.csv("EKG_pupsniu_analize.csv", sep = ";", header = TRUE) # Duomenu užkrovimas

set.seed(67)  # Nustatome sėklą (angl. seed), kad būtų įmanoma atkartoti rezultatus
df_sampled <- df %>%
  filter(!is.na(label)) %>%         # Pašaliname NA reikšmes klasės (angl. label) stulpelyje
  group_by(label) %>%               # Grupuojame duomenis pagal klasę (angl. label)
  slice_sample(n = 500, replace = FALSE) %>%  # Paimame po 500 atsitiktinių eilučių iš kiekvienos klasės (angl. label)
  ungroup()                         # Pašaliname grupavimą

write.csv(df_sampled, "csv/atrinkti_duomenys.csv", row.names = FALSE) # Išsaugome naują duomenų rinkinį į CSV failą