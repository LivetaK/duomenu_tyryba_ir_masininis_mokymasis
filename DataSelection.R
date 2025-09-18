library(dplyr)
library(randomForest)

if(!dir.exists("csv")) dir.create("csv")
df <- read.csv("EKG_pupsniu_analize.csv", sep = ";", header = TRUE) # Duomenu užkrovimas

set.seed(67)  # Nustatome sėklą (angl. seed), kad būtų įmanoma atkartoti rezultatus 67 buvusi
df_sampled <- df %>%
  filter(!is.na(label)) %>%         # Pašaliname NA reikšmes klasės (angl. label) stulpelyje
  group_by(label) %>%               # Grupuojame duomenis pagal klasę (angl. label)
  slice_sample(n = 500, replace = FALSE) %>%  # Paimame po 500 atsitiktinių eilučių iš kiekvienos klasės (angl. label)
  ungroup()                         # Pašaliname grupavimą

write.csv(df_sampled, "csv/atrinkti_duomenys.csv", row.names = FALSE) # Išsaugome naują duomenų rinkinį į CSV failą

num_vars <- names(df_sampled)[names(df_sampled) != "label"]

# Shapiro testas kiekvienam pozymiui, kiekvienai klasei, ar normaliai pasiskirstę duomenys.
check_normality <- function(var_name, df) {
  results <- df %>%
    group_by(label) %>%
    summarise(
      p_value = tryCatch(
        shapiro.test(.data[[var_name]])$p.value,
        error = function(e) NA
      ),
      .groups = "drop"
    )
  results$Variable <- var_name
  results
}

normality_results <- lapply(num_vars, check_normality, df = df_sampled) %>% # suformuojame duomenų rėmą
  bind_rows()

normality_results <- normality_results %>% # Pritaikome reikšmes gautiems rezultatams
  mutate(
    Normality = ifelse(p_value > 0.05, "Apytiksliai norm.", "Ne norm.")
  )

write.csv(normality_results, "normality_check_results.csv", row.names = FALSE) # Išsaugome normalumo testo rezultatus į CSV failą


kruskal_results <- sapply(num_vars, function(var) { # Kruskal-Wallis testas kiekvienam pozymiui, kadangi gavome, kad nei vienas is pozymiu nera normaliai pasiskirstes, negalime taikyti ANOVA F-test.
  formula <- as.formula(paste(var, "~ label"))
  kruskal.test(formula, data = df_sampled)$p.value
})

# Rusiuojame pagal svarba
kruskal_results <- sort(kruskal_results)
kruskal_results[1:6]  # Top 6 reiksmes, jas ir naudosime tolesniuose skaičiavimuose


