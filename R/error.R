#' Calcul de la racine de l'erreur quadratique moyenne (RMSE)
#'
#' @param actual le vecteur des "vraies" valeurs
#' @param predicted le vecteur des valeurs estimées
#' @return numeric : racine de l'erreur quadratique moyenne (RMSE)
#'
#' @export
#'
#' @examples
#' rmse(actual = c(35, 36), predicted = c(36, 34))
rmse <- function(actual, predicted) {
  return(sqrt(mse(actual, predicted)))
}

#' Calcul de l'erreur quadratique moyenne (MSE)
#'
#' @param actual le vecteur des "vraies" valeurs
#' @param predicted le vecteur des valeurs estimées
#' @return numeric : erreur quadratique moyenne (MSE)
#'
#' @export
#'
#' @examples
#' mse(actual = c(35, 36), predicted = c(36, 34))
mse <- function(actual, predicted) {
  return(mean((actual - predicted)^2))
}

#' Calcul de l'écart moyen absolu (MAE)
#'
#' @param actual le vecteur des "vraies" valeurs
#' @param predicted le vecteur des valeurs estimées
#' @return numeric : l'écart moyen absolu (MAE)
#'
#' @export
#'
#' @examples
#' mae(actual = c(35, 36), predicted = c(36, 34))
mae <- function(actual, predicted) {
  return(mean(abs(actual - predicted)))
}

#' Calcul de l'écart moyen absolu en pourcentage (MAPE)
#'
#' @param actual le vecteur des "vraies" valeurs
#' @param predicted le vecteur des valeurs estimées
#' @return numeric : l'écart moyen absolu en pourcentage (MAPE)
#'
#' @export
#'
#' @examples
#' mape(actual = c(35, 36), predicted = c(36, 34))
mape <- function(actual, predicted) {
  return(100 * mean(abs((actual - predicted) / actual)))
}

#' Helper function returning the desired metric function
#'
#' @param metric A character naming the expected metric function
#'
#' @return The function corresponding the character or NULL
#'
#' @export
#'
#' @examples
#' get_metric_fun("mae")
#' get_metric_fun("rmse")
#' get_metric_fun("mape")
#' get_metric_fun("mse")
get_metric_fun <- function(metric) {
  fun <- NULL
  if (metric == "mae") {
    fun <- mae
  } else if (metric == "rmse") {
    fun <- rmse
  } else if (metric == "mape") {
    fun <- mape
  } else if (metric == "mse") {
    fun <- mse
  }
  return(fun)
}

#' Returns the error between actual and predicted using metric
#'
#' @param actual A numeric vector of true values
#' @param predicted A numeric vector of estimated values
#' @param metric A character naming how to compute the error
#'
#' @return The numeric error
#'
#' @export
#'
#' @examples
#' error(actual = c(35, 36), predicted = c(36, 34), metric = "rmse")
error <- function(actual, predicted, metric = "rmse") {
  err_fun <- get_metric_fun(metric)
  return(err_fun(actual, predicted))
}

ind_error <- function(actual, predicted, metric) {
  fun <- get_metric_fun(metric)
  errs <- mapply(fun, actual, predicted)
  return(errs)
}

#' Computes the percentage of estimates' errors below a given threshold
#'
#' @param actual A numeric vector of true values
#' @param predicted A numeric vector of estimated values
#' @param cutoff A numeric: "acceptable" error in percent
#' @param metric A character naming how to compute the error
#'
#' @return The numeric percentage of prediction errors below the cutoff
#'
#' @export
#'
#' @examples
#' est_percent_below(actual = c(35, 36), predicted = c(36, 34), cutoff = 5,
#'                   metric = "mape")
est_percent_below <- function(actual = NULL, predicted = NULL, cutoff = 5,
                              metric = "mape",
                              errs = ind_error(actual, predicted, metric)) {
  return(100 * sum(errs <= cutoff) / length(errs))
}

#' Computes the accepted error in percentage needed to get a percentage of
#' acceptable estimates close to a target value
#'
#' @param actual A numeric vector of true values
#' @param predicted A numeric vector of estimated values
#' @param target A numeric: desired percent of "acceptable" estimated values
#' @param metric A character naming how to compute the error
#' @param errs A vector of numeric being the individual errors
#' @param window A list of integers: window where the objective lies
#'
#' @return The numeric percentage of prediction errors below the cutoff
#'
#' @export
#'
#' @examples
#' err_percent_above(actual = c(35, 36), predicted = c(36, 34), target = 80,
#'                   metric = "mape")
err_percent_above <- function(actual = NULL, predicted = NULL, target = 80,
                              metric = "mape",
                              errs = ind_error(actual, predicted, metric),
                              window = list(below = 0, above = 100)) {
  dist_tolerance <- 1
  window_min_size <- 1
  value <- round(mean(unlist(window)))
  est_percent <- est_percent_below(cutoff = value, errs = errs)
  dist_to_target <- target - est_percent
  if (abs(dist_to_target) < dist_tolerance) {
    return(value)
  } else {
    if (sign(dist_to_target) > 0) {
      window$above <- value
    } else {
      window$below <- value
    }
    if (window$above - window$below <= window_min_size) {
      return(mean(unlist(window)))
    }
    return(err_percent_above(target = target, metric = metric, errs = errs,
                             window = window))
  }
}

#' Returns the error introduced by a model
#'
#' @param model A model
#' @param data_test A data.frame
#' @param y_name A character naming the variable to explain
#' @param metric A character naming how to compute the error
#' @param est_below_cutoff "Acceptable" error in percent
#' @param err_above_cutoff Desired percent of "acceptable" estimated values
#'
#' @return The numeric error
#'
#' @export
#'
#' @examples
#' model_error(mod_lm, data_test, y_name = "SUBEX", metric = "rmse")
model_error <- function(model, data_test, y_name, metric = "rmse",
                        est_below_cutoff = 5, err_above_cutoff = 80,
                        error_only = FALSE) {
  actual <- data_test[, y_name]
  predicted <- predict_y(model, data_test)
  err <- error(actual, predicted, metric)
  mod_err <- list(err = err)
  if (!error_only) {
    mod_err$est_below <- est_percent_below(actual, predicted, est_below_cutoff)
    mod_err$err_above <- err_percent_above(actual, predicted, err_above_cutoff)
  }
  return(mod_err)
}
