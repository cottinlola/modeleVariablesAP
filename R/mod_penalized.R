#' Fits a GLM penalized model using k-fold cross-validation to tune the
#' lambda hyperparameter
#'
#' @param data A numeric dataset in matrix-convertible format
#' @param X_names A character vector of explainatory variables names
#' @param y_name A character naming the variable to explain
#' @param alpha A numeric to compromise between ridge (alpha=0) and lasso
#'              (alpha=1, default) penalization
#' @param type_measure  A character naming the error metric used for the
#'                      cross-validation (defaults to "mse")
#'
#' @return The fitted model
#'
#' @export
#'
#' @example
#' mod <- mod_penalized(data, y_name = "SUBEX")
#'
mod_penalized <- function(data, X_names = character(), y_name,
                          alpha = 1, type_measure = "mse") {
  if (length(X_names) != 0) {
    data <- data[, X_names]
  }
  X <- data[, -which(colnames(data) == y_name)]
  y <- data[, y_name]
  mod_en <- glmnet::cv.glmnet(x = as.matrix(X), y = y, alpha = alpha,
                              type.measure = type_measure)
  mod_en$glmnet.fit$cvm <- mod_en$cvm
  mod_en$glmnet.fit$nzero <- mod_en$nzero
  mod_en$glmnet.fit$lambda.min <- mod_en$lambda.min
  mod_en$glmnet.fit$lambda.1se <- mod_en$lambda.1se
  return(mod_en$glmnet.fit)
}

#' Returns the variables' names of non zero betas of an EN fitted model
#'
#' @param mod_en A fitted EN model
#' @param lambda A numeric corresponding to the regularization index desired
#'
#' @return A character vector of selected variables' names
#'
#' @export
#'
#' @example
#' vars <- mod_penalized_select_variables(mod_en, lambda)
#'
mod_penalized_select_variables <- function(mod_en, lambda) {
  lambda_idx <- which(mod_en$lambda == lambda)
  return(names(which(mod_en$beta[, lambda_idx] > 0)))
}
