#' MLReducedModel
#'
#' R6 class for multi-output regression using either polynomial regression or gradient boosting with bagging (via xgboost).
#'
#' @format An \code{R6Class} object with the following public methods:
#' \describe{
#'   \item{\code{new(X, Y, method = "polynomial", degree = 2, ...)}}{Constructor for the model. Initializes data and method.}
#'   \item{\code{fit()}}{Trains one model per output dimension in \code{Y}.}
#'   \item{\code{predict(X_new)}}{Predicts all output values from new input data \code{X_new}.}
#' }
#'
#' @examples
#' model <- MLReducedModel$new(X, Y, method = "polynomial")
#' model$fit()
#' Y_pred <- model$predict(X)
#'
#' @export
MLReducedModel <- R6::R6Class("MLReducedModel",
                              public = list(
                                X = NULL,
                                Y = NULL,
                                method = NULL,
                                degree = 2,
                                n_estimators = 10,
                                nrounds = 100,
                                subsample_rate = 0.8,
                                models = NULL,

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
                                },

                                fit = function() {
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
