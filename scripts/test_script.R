library(Algorithmique)
library(xgboost)
library(data.table)

df <- fread("data/dataset.csv")
X <- df[, .(X1, X2)]
Y <- df[, !c("X1", "X2"), with = FALSE]

splits <- train_test_calib_split(X, Y)
X_train <- splits$X_train
y_train <- splits$y_train
X_calib <- splits$X_calib
y_calib <- splits$y_calib
X_test <- splits$X_test
y_test <- splits$y_test
idx_calib <- splits$idx_calib
idx_test <- splits$idx_test

model <- MLReducedModel$new(X_train, y_train, method = "gradient_boosting")
model$fit()

y_pred_calib <- model$predict(X_calib)
y_pred_test  <- model$predict(X_test)

df1 <- copy(df)

for (j in seq_len(ncol(y_pred_calib))) {
  colname <- paste0("Y_calib_pred_", j)
  df1[[colname]] <- NA
  df1[[colname]][idx_calib] <- y_pred_calib[, j]
}

for (j in seq_len(ncol(y_pred_test))) {
  colname <- paste0("Y_test_pred_", j)
  df1[[colname]] <- NA
  df1[[colname]][idx_test] <- y_pred_test[, j]
}

fwrite(df1, "data/df1_with_predictions.csv")
cat("file saved: data/df1_with_predictions.csv\n")
