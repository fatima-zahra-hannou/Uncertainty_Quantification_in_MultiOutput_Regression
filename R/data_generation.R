#' Generate and Save a Non-linear Dataset (Optimized)
#'
#' This function generates a synthetic dataset with two input features (X1, X2) and a multi-dimensional
#' target variable Y using vectorized operations. The dataset is saved as a CSV file.
#'
#' @param Y_dim Integer. The number of output dimensions for Y.
#' @param n_samples Integer. Number of samples to generate (e.g. 1e6).
#' @param name_dataset_file String. Name of the output CSV file.
#'
#' @return None. Writes the dataset to a CSV file.
#'
#' @examples
#' generate_and_save_dataset_vectorized(24, 1e6, "dataset.csv")
#'
#' @export
generate_and_save_dataset_vectorized <- function(Y_dim, n_samples, name_dataset_file) {
  if (!require("data.table")) install.packages("data.table", dependencies = TRUE)
  library(data.table)

  abscissa_values <- seq(0, 10, length.out = Y_dim)

  first_non_linear_function <- function(X1, X2) {
    sin(X1) + cos(X2)
  }

  second_non_linear_function <- function(x) {
    result <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    mask <- x <= 7
    result[mask] <- ((exp(-x[mask]/10 + 1) + tan(x[mask])) * sin(2 * pi * x[mask]) + x[mask]^2) + tan(x[mask])
    result[!mask] <- ((exp(-x[!mask]/10 + 1) + tan(x[!mask])) * sin(2 * pi * x[!mask]) + x[!mask]^2) + tan(x[!mask]) - x[!mask]^2
    result
  }

  X1 <- runif(n_samples)
  X2 <- runif(n_samples)
  curve_values <- first_non_linear_function(X1, X2)
  shifted_abscissa <- matrix(rep(abscissa_values, each = n_samples), nrow = n_samples)
  shifted_abscissa <- shifted_abscissa + curve_values
  Y <- second_non_linear_function(shifted_abscissa)

  df <- data.table(X1 = X1, X2 = X2)
  colnames_Y <- paste0("Y", seq_len(Y_dim))
  df[, (colnames_Y) := as.data.table(Y)]

  # Création dossier si nécessaire
  dir.create(dirname(name_dataset_file), showWarnings = FALSE, recursive = TRUE)

  fwrite(df, name_dataset_file)
  cat("Dataset generated and saved to", name_dataset_file, "\n")
}
