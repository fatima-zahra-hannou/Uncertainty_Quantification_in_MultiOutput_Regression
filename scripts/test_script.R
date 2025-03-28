library(Algorithmique)
library(xgboost)
library(data.table)
library(R6)
##################### 1 : data Loading
# Load dataset
df <- fread("data/dataset.csv")
X <- df[, .(X1, X2)]
Y <- df[, !c("X1", "X2"), with = FALSE]

print("the dimension of X is:")
print(dim(X))

print("the dimension of Y is:")
print(dim(Y))

##################### 2 : data Splitting

# Define split parameters
n_train <- 10000
n_test <- 1000
n_calib <- 2000

# Perform data split
splits <- train_test_calib_split(X, Y, n_train = n_train, n_test = n_test, n_calib = n_calib)

# Extract split datasets
X_train <- splits$X_train
y_train <- splits$y_train
X_calib <- splits$X_calib
y_calib <- splits$y_calib
X_test <- splits$X_test
y_test <- splits$y_test
idx_calib <- splits$idx_calib
idx_test <- splits$idx_test

##################### 3 : Model building and training

# Building our model
model <- MLModel$new(X_train, y_train, method = "gradient_boosting")
model$fit()

# Ensure the "Saved_Models" directory exists
dir.create("Saved_Models", showWarnings = FALSE, recursive = TRUE)

# Save the model
saveRDS(model, file = "Saved_Models/MLModel_trained.rds")

##################### 4 : Uncertainties quantification


###### Test

# Loading our pretrained model
loaded_model <- readRDS("Saved_Models/MLModel_trained.rds")

# Initialize the ModelUncertainties class
uncertainty_model <- ModelUncertainties$new(
  model = loaded_model,
  X_calibration = X_calib,
  Y_calibration = y_calib,
  uncertainty_method = "Beta_opt",
  Global_alpha = 0.8
)

# Fit the uncertainty method on the model (compute the Beta quantiles)
uncertainty_model$fit()

# Get the prediction bounds
pred_bounds_test <- uncertainty_model$predict(X_test)

y_test_lower <- pred_bounds_test$y_lower
y_test_upper <- pred_bounds_test$y_upper

# Compute the empirical coverage obtained of the test data

Empirical_simultaneous_coverage <- simultaneous_coverage(y_test, y_test_lower, y_test_upper)

##################### 5 : Plot visualization

# making prediciton on the test set

y_pred_test <- loaded_model$predict(X_test)

# Define the x-axis values (24 points evenly spaced between 0 and 10)
x_values <- seq(0, 10, length.out = 24)

# Extract the relevant row (10th)
y_values_pred <- y_pred_test[10, ]  # Predicted curve (red)
y_values_true <- y_test[10, ]       # True curve (black)
y_up <- y_test_upper[10, ]     # Upper bound (green dashed)
y_low <- y_test_lower[10, ]     # Lower bound (green dashed)

# Define the x-axis values (24 points evenly spaced between 0 and 10)
x_values <- seq(0, 10, length.out = 24)

# Plot predicted curve in red
plot(x_values, y_values_pred, type = "l", col = "red", lwd = 2,
     xlab = "X", ylab = "Value", main = "Prediction vs True Curve with Bounds")

# Add true curve in black
lines(x_values, y_values_true, col = "black", lwd = 2)

# Add upper and lower bounds in green (dashed)
lines(x_values, y_up, col = "green", lwd = 2, lty = 2)  # Dashed line
lines(x_values, y_low, col = "green", lwd = 2, lty = 2)  # Dashed line

# Add legend
legend("topright", legend = c("Predicted", "True", "Bounds"),
       col = c("red", "black", "green"), lwd = 2, lty = c(1, 1, 2))


# Ckeck #####################################

# Making prediction on our calibration data
y_pred_calib  <- loaded_model$predict(X_calib)

absolute_residual_scores <- function(y_pred, y_true) {
  return(abs(y_pred - y_true))  # Returning a matrix or vector
}

S <- absolute_residual_scores(y_pred_calib, y_calib)

get_sorted_scores <- function(Scores) {
  # Check if Scores is a data.table, if so use proper indexing
  if (inherits(Scores, "data.table")) {
    n <- nrow(Scores)
    n_dim <- ncol(Scores)
    sorted_scores <- data.table(matrix(0, nrow = n, ncol = n_dim))

    for (i in seq_len(n_dim)) {
      sorted_scores[[i]] <- sort(Scores[[i]])
    }
    return(sorted_scores)
  } else {
    # If Scores is a matrix, proceed with matrix sorting
    n <- nrow(Scores)
    n_dim <- ncol(Scores)
    sorted_scores <- matrix(0, nrow = n, ncol = n_dim)

    for (i in seq_len(n_dim)) {
      sorted_scores[, i] <- sort(Scores[, i])
    }
    return(sorted_scores)
  }
}

sorted_S <- get_sorted_scores(S)

get_Beta_quantiles <- function(sorted_Scores, Beta) {
  sorted_Scores <- as.matrix(sorted_Scores)  # Ensure it's a matrix

  n <- nrow(sorted_Scores)
  n_dim <- ncol(sorted_Scores)
  quantile_values <- numeric(n_dim)

  for (i in seq_len(n_dim)) {
    # Calculate rank correctly, ensuring it's always within valid bounds
    rank <- min(max(ceiling((1 - Beta) * n), 1), n)  # Ensures it's in [1, n]

    # Extract the Beta quantile value for the current dimension
    quantile_values[i] <- sorted_Scores[rank, i]
  }

  return(quantile_values)
}
# choosing a local beta
Beta = 0.1

# New try with new function

get_prediction_bounds <- function(quantile, y_pred) {
  quantile <- matrix(quantile, nrow = 1, ncol = length(quantile))  # Ensure row vector

  # Repeat the quantile row for all samples (row-wise expansion)
  quantile <- matrix(rep(quantile, each = nrow(y_pred)), nrow = nrow(y_pred), byrow = TRUE)

  y_lower <- y_pred - quantile
  y_upper <- y_pred + quantile

  return(list(y_lower = y_lower, y_upper = y_upper))
}

# new function

get_prediction_bounds <- function(quantile, y_pred) {
  # Ensure quantile is a vector
  quantile <- as.vector(quantile)

  # Expand quantile into a matrix with the same number of rows as y_pred
  quantile_matrix <- matrix(quantile, nrow = nrow(y_pred), ncol = length(quantile), byrow = TRUE)

  # Compute the prediction bounds
  y_lower <- y_pred - quantile_matrix
  y_upper <- y_pred + quantile_matrix

  return(list(y_lower = y_lower, y_upper = y_upper))
}

bounds <- get_prediction_bounds(Beta_quantile, y_pred_calib)

dim(bounds$y_lower)
dim(bounds$y_upper)

size <- as.matrix(bounds$y_upper - bounds$y_lower)

dim(size)

# Check to see if the calculs are correct (we should obtain the same value repeated)
size[7,1]

size[700,1]

# Optimization problem

y_calibration <- y_calib

Eval_simultaneous_coverage_beta <- function(Beta) {
  # Get Beta quantiles
  Beta_quantile <- get_Beta_quantiles(sorted_S, Beta)

  # Get the corresponding lower and upper prediction bounds
  bounds <- get_prediction_bounds(Beta_quantile, y_pred_calib)

  # Compute the empirical simultaneous coverage
  Sim_coverage_Beta <- simultaneous_coverage(y_calibration, bounds$y_lower, bounds$y_upper)

  return(Sim_coverage_Beta)
}

# fixing global alpha value
alpha <- 0.1

Eval_simultaneous_coverage_beta(0.1/100000000000)

beta_optim <- dichotomie(target = 1 - alpha, xmin = 0, xmax = alpha, n_iter = 10, Eval_simultaneous_coverage_beta)
}

# test
beta = 0
Beta_quantile <- get_Beta_quantiles(sorted_S, Beta=beta)
Beta_quantile

bounds <- get_prediction_bounds(Beta_quantile, y_pred_calib)

dim(bounds$y_lower)
dim(bounds$y_upper)

size <- as.matrix(bounds$y_upper - bounds$y_lower)

dim(size)

# Check to see if the calculs are correct (we should obtain the same value repeated)
size[,1]/2

dim(sorted_S)
sorted_S[2000,]

#############################################


