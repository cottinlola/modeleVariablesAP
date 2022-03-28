#' Fits a GLM penalized model using k-fold cross-validation to tune the
#' lambda hyperparameter
#'
#' @param X A numeric dataset in matrix-convertible format
#' @param y A numeric vector
#' @param vars.names A character vector
#' @param alpha A numeric to balance betwwen ridge (alpha=0) and lasso
#'              (alpha=1, default) penalization
#' @param type.measure  A character naming the error metric used for the
#'                      cross-validation (defaults to "rmse")
#'
#' @return The fitted model
#'
penalized_models <- function (X, y, vars.names = character(), alpha = 1,
                              type.measure = "rmse") {
  if (length(vars.names) == 0) {
    X <- X[, vars.names]
  }
  mod_en <- glmnet::cv.glmnet(x = as.matrix(X), y = y, alpha = alpha,
                              type.measure = type.measure)
  mod_en$glmnet.fit$cvm <- mod_en$cvm
  mod_en$glmnet.fit$nzero <- mod_en$nzero
  mod_en$glmnet.fit$lambda.min <- mod_en$lambda.min
  mod_en$glmnet.fit$lambda.1se <- mod_en$lambda.1se
  return(mod_en$glmnet.fit)
}
