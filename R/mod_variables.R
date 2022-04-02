model_variables <- function(model, ...) UseMethod("model_variables")

#' Returns the variables' names of non zero betas of an EN fitted model
#'
#' @param mod_en A fitted EN model
#' @param lambda A numeric corresponding to the desired regularization index
#'
#' @return A character vector of selected variables' names
#'
#' @export
#'
#' @example
#' vars <- model_variables(mod_en, lambda)
#'
model_variables.glmnet <- function(model, lambda = NULL) {
  if (is.null(lambda)) {
    lambda <- model$lambda.min
  }
  lambda_idx <- which(model$lambda == lambda)
  return(names(which(model$beta[, lambda_idx] > 0)))
}
model_variables.cv.glmnet <- model_variables.glmnet

#' Returns the variables' names of a LM fitted model
#'
#' @param mod_en A fitted LM model
#'
#' @return A character vector of variables' names
#'
#' @export
#'
#' @example
#' vars <- model_variables(mod_lm)
#'
model_variables.lm <- function(model, lambda = NULL) {
  vars <- all.vars(terms(model))
  return(vars[2:length(vars)])
}

models_variables <- function(models, names = NULL, lambda = NULL) {
  if (is.null(names)) {
    names <- names(models)
  }
  vars <- lapply(models, function(model) model_variables(model, lambda))
  names(vars) <- names
  return(vars)
}