library(dplyr)
library(tidyr)
library(ggplot2)


df_mean_sd  <- read.csv("csv/normalized_mean_sd.csv")
df_min_max  <- read.csv("csv/normalized_min_max.csv")


med_vals_minmax <- df_min_max %>%
  mutate(label = as.factor(label)) %>%
  group_by(label) %>%
  summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-label, names_to = "Pozymis", values_to = "Reiksme")

ggplot(med_vals_minmax, aes(x = label, y = Reiksme, fill = Pozymis)) +
  geom_col(position = "dodge") +
  labs(
    title = "Normuotos medianos pagal pūpsnio klasę (Min–Max)",
    x = "Pūpsnio klasė", y = "Normuota reikšmė (mediana)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")


med_vals_z <- df_mean_sd %>%
  mutate(label = as.factor(label)) %>%
  group_by(label) %>%
  summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-label, names_to = "Pozymis", values_to = "Reiksme")

ggplot(med_vals_z, aes(x = label, y = Reiksme, fill = Pozymis)) +
  geom_col(position = "dodge") +
  labs(
    title = "Normuotos medianos pagal pūpsnio klasę (Vidurkis–SD)",
    x = "Pūpsnio klasė", y = "Normuota reikšmė (mediana)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")
