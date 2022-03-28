#' Fits a GLM penalized model using k-fold cross-validation to tune the
#' lambda hyperparameter
#'
#' @param data A numeric dataset in matrix-convertible format
#' @param X.vars.names A character vector of explainatory variables names
#' @param y.var.name A character naming the variable to explain
#' @param alpha A numeric to balance betwwen ridge (alpha=0) and lasso
#'              (alpha=1, default) penalization
#' @param type.measure  A character naming the error metric used for the
#'                      cross-validation (defaults to "rmse")
#'
#' @return The fitted model
#'
#' @example
#' mod <- mod_penalized(data, y.var.name = "SUBEX")
#'
mod_penalized <- function (data, X.vars.names = character(), y.var.name,
                              alpha = 1, type.measure = "rmse") {
  if (length(X.vars.names) == 0) {
    data <- data[, X.vars.names]
  }
  X <- data[, -y.var.name]
  y <- data[, y.var.name]
  mod_en <- glmnet::cv.glmnet(x = as.matrix(X), y = y, alpha = alpha,
                              type.measure = type.measure)
  mod_en$glmnet.fit$cvm <- mod_en$cvm
  mod_en$glmnet.fit$nzero <- mod_en$nzero
  mod_en$glmnet.fit$lambda.min <- mod_en$lambda.min
  mod_en$glmnet.fit$lambda.1se <- mod_en$lambda.1se
  return(mod_en$glmnet.fit)
}
