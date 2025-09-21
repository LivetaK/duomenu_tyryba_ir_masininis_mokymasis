library(dplyr)
library(tidyr)
library(ggplot2)

# 1) Nuskaitymas
df <- read.csv("csv/clean_data.csv")

# 2) Min–Max normalizacija išlaikant 'label'
min_max <- df %>%
  mutate(across(-label, ~ (.-min(.)) / (max(.)-min(.)), .names = "{.col}"))

# 3) Paverčiame label į faktorius (tvarkingesnė ašis)
min_max <- min_max %>% mutate(label = as.factor(label))

# 4) Kiekvienos klasės vidurkių skaičiavimas
avg_vals <- min_max %>%
  group_by(label) %>%
  summarise(across(-any_of("label"), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

# 5) Duomenų pavertimas į long formatą
avg_long <- avg_vals %>%
  pivot_longer(-label, names_to = "Pozymis", values_to = "Reiksme")

# 6) Stulpelinė diagrama su „Set2“ palete
ggplot(avg_long, aes(x = label, y = Reiksme, fill = Pozymis)) +
  geom_col(position = "dodge") +
  labs(title = "Normuotos reikšmės pagal pūpsnio klasę (Min–Max)",
       x = "Pūpsnio klasė", y = "Normuota reikšmė") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")  # Naudoja ColorBrewer Set2 paletę
