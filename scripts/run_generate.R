source("R/data_generation.R")

generate_and_save_dataset_vectorized(
  Y_dim = 24,
  n_samples = 1e6,
  name_dataset_file = "data/dataset.csv"
)

