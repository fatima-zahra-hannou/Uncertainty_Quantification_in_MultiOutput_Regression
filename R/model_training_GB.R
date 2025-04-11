#' @importFrom xgboost xgb.DMatrix
#' @importFrom caret train
#' @importFrom data.table as.data.table fwrite
#' @importFrom R6 R6Class
#' @importFrom stats lm poly runif
#' @importFrom utils install.packages
#'
#' @title MLModel Class
#' @description R6 class for multi-output regression using either polynomial regression or gradient boosting with bagging (via xgboost).
#'
#' @field X A data frame or matrix of input features used for training. Must be of size (n_samples, n_features).
#' @field Y A matrix or data frame of target values for training. Each column corresponds to an output dimension.
#' @field method A string indicating the regression method: "polynomial" or "gradient_boosting".
#' @field degree Degree of the polynomial used when method = "polynomial". Default is 2.
#' @field n_estimators Number of estimators (models) used in gradient boosting. Default is 10.
#' @field nrounds Number of boosting rounds for each xgboost model. Default is 100.
#' @field subsample_rate Proportion of training data to subsample for bagging in gradient boosting. Default is 0.8.
#' @field models A list to store trained models for each output dimension.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(X, Y, method = "polynomial", degree = 2, n_estimators = 10, nrounds = 100, subsample_rate = 0.8)}}{Constructor. Initializes training data, method, and model hyperparameters.}
#'   \item{\code{fit()}}{Trains one model per output dimension in \code{Y} using the specified method.}
#'   \item{\code{predict(X_new)}}{Returns predictions from trained models given new input data.}
#' }
#'
#' @examples
#' X_train <- matrix(rnorm(100), nrow = 10, ncol = 10)  # 10 samples, 10 features
#' Y_train <- matrix(rnorm(30), nrow = 10, ncol = 3)  # 10 samples, 10 features, 3 output dimensions
#' X <- X_train
#' Y <- Y_train
#' model <- MLModel$new(X, Y, method = "gradient_boosting")
#' model$fit()
#' Y_pred <- model$predict(X)
#'
#' @export
MLModel <- R6::R6Class("MLModel",
                       public = list(
                         X = NULL,
                         Y = NULL,
                         method = NULL,
                         degree = 2,
                         n_estimators = 10,
                         nrounds = 100,
                         subsample_rate = 0.8,
                         models = NULL,

                         #' @description Initializes the model with input features, target values, and hyperparameters.
                         #' @param X A data frame or matrix of features.
                         #' @param Y A matrix or data frame of target values.
                         #' @param method Regression method to use: "polynomial" or "gradient_boosting".
                         #' @param degree Degree of the polynomial (if applicable).
                         #' @param n_estimators Number of bagging estimators.
                         #' @param nrounds Number of boosting rounds.
                         #' @param subsample_rate Sampling rate for bootstrapping in gradient boosting.
                         initialize = function(X, Y, method = "polynomial", degree = 2,
                                               n_estimators = 10, nrounds = 100, subsample_rate = 0.8) {
                           self$X <- X
                           self$Y <- Y
                           self$method <- method
                           self$degree <- degree
                           self$n_estimators <- n_estimators
                           self$nrounds <- nrounds
                           self$subsample_rate <- subsample_rate
                           self$models <- list()

                           # Ensure Y is always a matrix (if it's a vector, convert it to a column matrix)
                           if (is.vector(self$Y)) {
                             self$Y <- matrix(self$Y, ncol = 1)}
                         },

                         #' @description Fit the model. Trains one model per output dimension in Y.
                         fit = function() {

                           # Handle multi-dimensional Y properly
                           if (is.matrix(self$Y) || is.data.frame(self$Y)) {
                             num_columns <- ncol(self$Y)
                           } else {
                             num_columns <- length(self$Y)  # Fallback for vectors
                           }

                           for (j in seq_len(ncol(self$Y))) {
                             y_target <- self$Y[, j]
                             if (self$method == "polynomial") {
                               formula <- as.formula(paste("y_target ~ poly(", paste(colnames(self$X), collapse = "+"), ",", self$degree, ", raw=TRUE)"))
                               model <- caret::train(formula, data = cbind(self$X, y_target), method = "lm")
                               self$models[[j]] <- model
                             } else if (self$method == "gradient_boosting") {
                               models_j <- list()
                               for (b in seq_len(self$n_estimators)) {
                                 idx <- sample(1:nrow(self$X), size = round(nrow(self$X) * self$subsample_rate), replace = TRUE)
                                 dtrain <- xgb.DMatrix(data = as.matrix(self$X[idx, ]), label = y_target[idx])
                                 model <- xgboost::xgboost(data = dtrain, nrounds = self$nrounds,
                                                           objective = "reg:squarederror", verbose = 0)
                                 models_j[[b]] <- model
                               }
                               self$models[[j]] <- models_j
                             } else {
                               stop("Unknown method")
                             }
                           }
                           names(self$models) <- colnames(self$Y)
                         },

                         #' @description Predict the target values for new input data.
                         #' @param X_new A matrix or data frame of new input features for prediction.
                         #' The number of rows should match the number of samples in the model's training data.
                         #' @return A matrix of predicted values. Each column corresponds to an output dimension.
                         predict = function(X_new) {
                           n_outputs <- length(self$models)
                           Y_pred <- matrix(NA, nrow = nrow(X_new), ncol = n_outputs)
                           for (j in seq_len(n_outputs)) {
                             if (self$method == "polynomial") {
                               Y_pred[, j] <- predict(self$models[[j]], X_new)
                             } else if (self$method == "gradient_boosting") {
                               preds <- sapply(self$models[[j]], function(m) predict(m, newdata = as.matrix(X_new)))
                               Y_pred[, j] <- rowMeans(preds)
                             }
                           }
                           colnames(Y_pred) <- names(self$models)
                           return(Y_pred)
                         }
                       )
)
