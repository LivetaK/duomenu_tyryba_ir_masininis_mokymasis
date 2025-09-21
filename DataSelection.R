library(dplyr)

if (!dir.exists("csv")) dir.create("csv")

df <- read.csv("EKG_pupsniu_analize.csv", sep = ";", dec = ",", header = TRUE, na.strings = c("", "NA"))

df <- df %>% filter(!is.na(label)) # Pasaliname eiles su NA label reikšmėmis
df$label <- as.factor(df$label)

# Paverciame visas kintamųjų reikšmes į skaitines
feature_cols <- setdiff(names(df), "label")
df[feature_cols] <- lapply(df[feature_cols], function(x) {
  if (is.numeric(x)) return(x)
  as.numeric(gsub(",", ".", as.character(x)))
})

# Paliekame tik tas eilutes, kuriose nėra NA ar Inf reikšmių
df_clean <- df %>%
  filter(if_all(all_of(feature_cols), ~ is.finite(.) & !is.na(.)))

# Išsaugome išvalytus duomenis
write.csv(df_clean, "csv/atrinkti_duomenys.csv", row.names = FALSE)

# Nustatome, kurie požymiai yra kiekybiniai
num_vars <- setdiff(names(df_clean)[vapply(df_clean, is.numeric, logical(1))], "label")

# Kruskalio–Walliso kriterijus ir eta kvadrato efektų dydis
kruskal_results <- sapply(num_vars, function(var) {
  formula <- as.formula(paste(var, "~ label"))
  test <- kruskal.test(formula, data = df_clean)
  H <- test$statistic
  eta2 <- H / (nrow(df_clean) - 1)
  c(p_value = test$p.value, eta2 = eta2)
})

kruskal_results <- t(kruskal_results) %>% as.data.frame()
colnames(kruskal_results) <- c("p_value", "eta2")  # Nustatome kintamuju vardus rezultatu isvedimui
kruskal_results$Variable <- rownames(kruskal_results)

# Pasirenkame 6 rodiklius pagal eta kvadrato reikšmę (mažiausias požymių persidengimas pagal klasę) ir juos atspausdiname
top_kruskal <- kruskal_results %>%
  arrange(desc(eta2)) %>%
  head(6)

top_kruskal

# Sudarome koreliacijos matricą, tam, kad įvertintume ar nėra stipriai koreliuotų požymių
top6_vars <- top_kruskal$Variable
cor_matrix <- cor(df_clean[top6_vars], use = "complete.obs")
cor_matrix