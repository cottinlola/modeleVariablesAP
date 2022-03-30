#' Fits a GLM penalized model using k-fold cross-validation to tune the
#' alpha and lambda hyperparameters
#'
#' @param data A numeric dataset in matrix-convertible format
#' @param x_names A character vector of explainatory variables names
#' @param y_name A character naming the variable to explain
#' @param alpha A numeric vector of alpha to use for the cross-validation
#' @param cv_strat  A character telling which alpha should be consider optimal
#'                      "min": minimize the error
#'                      "1se":  selects less variables while allowing a greater
#'                              error (min + 1se)
#' @param n_folds An integer giving the number of folds tu use in k-folds cv
#' @param type_measure  A character naming the error metric used for the
#'                      cross-validation (defaults to "mse")
#'
#' @return The fitted model
#'
#' @export
#'
#' @example
#' mod <- cv_mod_penalized(data, y_name = "SUBEX")
#'
mod_cv_penalized <- function(data, x_names = character(), y_name,
                             alpha = seq(0, 1, by = .2), cv_strat = "min",
                             n_folds = 10, type_measure = "mse") {
  # assign every observation to a fold
  folds_id <- sample(rep(seq(n_folds), length = nrow(data)))
  # for every alpha to consider compute its error over the different folds
  err <- lapply(alpha, function(a) folds_err(data, x_names, y_name,
                                             a, cv_strat, n_folds,
                                             type_measure, folds_id))
  names(err) <- alpha
  err <- do.call(rbind, err)
  # retrieves the alpha minimizing the error
  alpha_min <- max(alpha[which(err$mean == min(err$mean))])
  # retrieves the greatest alpha keeping the error "close" to its minimum
  alpha_1se <- max(alpha[which(err$mean <= err$mean[[alpha_min]] +
    err$sd[[alpha_min]])])
  alpha_opt <- if (cv_strat == "min") alpha_min else alpha_1se
  # fitting the final penalized model using the "optimal" alpha
  mod_full <- mod_penalized(data, x_names, y_name, alpha_opt, type_measure)
  mod_full$alpha <- alpha_opt
  return(mod_full)
}

#' Computes the mean and deviation of the error over the folds
#'
#' @param data A numeric dataset in matrix-convertible format
#' @param x_names A character vector of explainatory variables names
#' @param y_name A character naming the variable to explain
#' @param alpha A numeric to compromise between ridge and lasso penalization
#' @param cv_strat  A character telling which alpha should be consider optimal
#' @param n_folds An integer giving the number of folds tu use in k-folds cv
#' @param type_measure  A character naming the error metric used for the cv
#' @param folds_id  An integer vector specifing to which fold an observation
#'                  belongs
#'
#' @return  A data.frame with the mean and the deviation of the error over the
#'          folds
#'
#' @example
#' err <- folds_err(data, ...)
#'
#' @noRd
#'
folds_err <- function(data, x_names, y_name, alpha, cv_strat, n_folds,
                      type_measure, folds_id) {
  alpha_err <- sapply(1:n_folds, function(fid)
      fold_err(data, x_names, y_name, alpha, cv_strat, type_measure,
               folds_id, fid))
  # returns error mean and sd over the folds
  return(data.frame(mean = mean(alpha_err), sd = sd(alpha_err)))
}

#' Computes the error for a fold between the estimated and true values
#'
#' @param data A numeric dataset in matrix-convertible format
#' @param x_names A character vector of explainatory variables names
#' @param y_name A character naming the variable to explain
#' @param alpha A numeric to compromise between ridge and lasso penalization
#' @param cv_strat  A character telling which alpha should be consider optimal
#' @param type_measure  A character naming the error metric used for the cv
#' @param folds_id  An integer vector specifing to which fold an observation
#'                  belongs
#' @param fid A integer representing the fold id to use as validation data
#'
#' @return A numeric being the error
#'
#' @example
#' err <- fold_err(data, ...)
#'
#' @noRd
#'
fold_err <- function(data, x_names, y_name, alpha, cv_strat,
                     type_measure, folds_id, fid) {
  whichs <- folds_id == fid
  # restrict training data to observations not assigned to the fold fid
  data_train <- data[!whichs, ]
  # restrict test data to observations assigned to the fold fid
  data_test <- data[whichs, ]
  # fit a penalized model
  mod_en <- mod_penalized(data_train, x_names, y_name, alpha = alpha,
                          type_measure = type_measure)
  # select variables' names for a given lambda
  vars_names <- mod_penalized_select_variables(
    mod_en, mod_en[[paste("lambda", cv_strat, sep = ".")]])
  # fit linear model restricted to the selected variables
  mod_lm <- mod_lineaire(data_train, vars_names, y_name)
  err <- model_error(mod_lm, data_test, y_name, metric = type_measure)
  return(err$error)
}
