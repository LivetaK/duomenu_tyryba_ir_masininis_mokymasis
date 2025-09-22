library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv("csv/clean_data.csv")


min_max <- df %>%
  mutate(across(-label, ~ (.-min(.)) / (max(.)-min(.)), .names = "{.col}"))

#Paverčiame label į faktorius (tvarkingesnė ašis)
min_max <- min_max %>% mutate(label = as.factor(label))

#Kiekvienos klasės vidurkių skaičiavimas
avg_vals <- min_max %>%
  group_by(label) %>%
  summarise(across(-any_of("label"), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

#Duomenų pavertimas į long formatą dėl ggplot2
avg_long <- avg_vals %>%
  pivot_longer(-label, names_to = "Pozymis", values_to = "Reiksme")


ggplot(avg_long, aes(x = label, y = Reiksme, fill = Pozymis)) +
  geom_col(position = "dodge") +
  labs(title = "Normuotos reikšmės pagal pūpsnio klasę (Min–Max)",
       x = "Pūpsnio klasė", y = "Normuota reikšmė") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")




label_col <- "label"
pozymiai <- df[ , !(names(df) %in% label_col)]
labels <- df[[label_col]]

#Vidurkio–dispersijos normalizacija
vidurkis_sd <- as.data.frame(scale(pozymiai))
vidurkis_sd$label <- labels


vidurkis_sd <- vidurkis_sd %>% mutate(label = as.factor(label))

#Apskaičiuojame kiekvienos klasės vidurkius
avg_vals_z <- vidurkis_sd %>%
  group_by(label) %>%
  summarise(across(-any_of("label"), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

#Duomenis paverčiame į long formatą
avg_long_z <- avg_vals_z %>%
  pivot_longer(-label, names_to = "Pozymis", values_to = "Reiksme")


ggplot(avg_long_z, aes(x = label, y = Reiksme, fill = Pozymis)) +
  geom_col(position = "dodge") +
  labs(title = "Normuotos reikšmės pagal pūpsnio klasę (Vidurkis–SD)",
       x = "Pūpsnio klasė", y = "Normuota reikšmė") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")
