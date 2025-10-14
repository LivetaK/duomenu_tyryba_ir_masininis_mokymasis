if (!require(Rtsne)) install.packages("Rtsne")
library(Rtsne)


if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
# 1. Įkeliame duomenis


ekg_data <- read.csv("../normalized_NO_missing_1498_6.csv")

# 2. Atskiriame požymius ir klases
features <- ekg_data[, -ncol(ekg_data)]
labels   <- ekg_data[,  ncol(ekg_data)]

# Konvertuojame požymius į skaitinę matricą
features_numeric <- as.matrix(sapply(features, as.numeric))

# 3. Sukuriame t-SNE taikymo ir braižymo funkciją
apply_and_plot_t_sne <- function (data, parameters, label_col, title) {
  # Ištraukiame hiperparametrus
  perplexity <- as.numeric(parameters$perplexity)
  pca_init <- as.logical(parameters$pca_init)
  max_iter <- as.numeric(parameters$max_iter)
  eta <- as.numeric(parameters$eta)
  exaggeration_factor <- as.numeric(parameters$exaggeration_factor)

  # Pritaikome t-SNE
  tsne_result <- Rtsne(
    data,
    perplexity = perplexity,
    pca = pca_init,
    max_iter = max_iter,
    eta = eta,
    exaggeration_factor = exaggeration_factor,
    normalize = FALSE
  )

  # Išsaugome taškų koordinates
  tsne_coords <- tsne_result$Y
  colnames(tsne_coords) <- c("dim_1", "dim_2")

  # Pridedame klases
  tsne_df <- data.frame(tsne_coords, Label = as.factor(label_col))
  max_range <- max(abs(range(tsne_df$dim_1)), abs(range(tsne_df$dim_2)))

  # Braižome grafiką
  ggplot(tsne_df, aes(x = dim_1, y = dim_2, color = Label)) +
    geom_point(size = 2) +
    labs(
      title = title,
      x = "t-SNE 1 dimensija",
      y = "t-SNE 2 dimensija",
      caption = paste0(
        "Parametrai: distance metric = ", "euclidean",
        ", perplexity = ", perplexity,
        ", PCA initialisation = ", pca_init,
        ", number of iterations = ", max_iter,
        ", exaggeration factor = ", exaggeration_factor,
        ", learning rate = ", eta
      )
    ) +
    scale_x_continuous(limits = c(-max_range, max_range)) +
    scale_y_continuous(limits = c(-max_range, max_range)) +
    coord_equal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      plot.caption = element_text(hjust = 0.5, size = 12),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      axis.title = element_text(size = 16)
    )
}

# 4. Nustatome hiperparametrus ir paleidžiame t-SNE
set.seed(1000)
params <- list(
  perplexity = 30,
  pca_init = TRUE,
  max_iter = 1000,
  eta = 200,
  exaggeration_factor = 12
)

apply_and_plot_t_sne(
  data = features_numeric,
  parameters = params,
  label_col = labels,
  title = "t-SNE vizualizacija atrinktų požymių duomenų aibei (normuota)"
)
