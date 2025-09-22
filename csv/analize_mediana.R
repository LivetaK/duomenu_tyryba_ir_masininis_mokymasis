library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv("csv/clean_data.csv")

# Min–Max normalizacija
min_max <- df %>%
  mutate(across(-label, ~ (.-min(.)) / (max(.)-min(.)), .names = "{.col}")) %>%
  mutate(label = as.factor(label))

# Medianų skaičiavimas
med_vals <- min_max %>%
  group_by(label) %>%
  summarise(across(-any_of("label"), ~ median(.x, na.rm = TRUE)),
            .groups = "drop")

med_long <- med_vals %>%
  pivot_longer(-label, names_to = "Pozymis", values_to = "Reiksme")

# Diagrama
ggplot(med_long, aes(x = label, y = Reiksme, fill = Pozymis)) +
  geom_col(position = "dodge") +
  labs(title = "Normuotos medianos pagal pūpsnio klasę (Min–Max)",
       x = "Pūpsnio klasė", y = "Normuota reikšmė (mediana)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")


# Požymių ir etikečių atskyrimas
label_col <- "label"
pozymiai <- df[ , !(names(df) %in% label_col)]
labels <- df[[label_col]]

# Z-score normalizacija
vidurkis_sd <- as.data.frame(scale(pozymiai))
vidurkis_sd$label <- labels
vidurkis_sd <- vidurkis_sd %>% mutate(label = as.factor(label))

# Medianų skaičiavimas
med_vals_z <- vidurkis_sd %>%
  group_by(label) %>%
  summarise(across(-any_of("label"), ~ median(.x, na.rm = TRUE)),
            .groups = "drop")

med_long_z <- med_vals_z %>%
  pivot_longer(-label, names_to = "Pozymis", values_to = "Reiksme")

# Diagrama
ggplot(med_long_z, aes(x = label, y = Reiksme, fill = Pozymis)) +
  geom_col(position = "dodge") +
  labs(title = "Normuotos medianos pagal pūpsnio klasę (Vidurkis–SD)",
       x = "Pūpsnio klasė", y = "Normuota reikšmė (mediana)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")
