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
error <- function (actual, predicted, metric = "rmse") {
  err_fun <- get_metric_fun(metric)
  return(err_fun(actual, predicted))
}

#' Computes the percentage of prediction errors below a given threshold
#'
#' @param actual A numeric vector of true values
#' @param predicted A numeric vector of estimated values
#' @param cutoff A numeric being the threshold value
#' @param metric A character naming how to compute the error
#'
#' @return The numeric percentage of prediction errors below the cutoff
#'
#' @export
#'
#' @examples
#' percent_err_below(actual = c(35, 36), predicted = c(36, 34), cutoff = 5,
#'                   metric = "mape")
percent_err_below <- function(actual, predicted, cutoff = 5, metric = "mape") {
  fun <- get_metric_fun(metric)
  errs <- mapply(fun, actual, predicted)
  return(100 * sum(errs <= cutoff) / length(actual))
}

#' Returns the error introduced by a model
#'
#' @param model A model
#' @param data_test A data.frame
#' @param y_name A character naming the variable to explain
#' @param metric A character naming how to compute the error
#'
#' @return The numeric error
#'
#' @export
#'
#' @examples
#' model_error(mod_lm, data_test, y_name = "SUBEX", metric = "rmse")
model_error <- function(model, data_test, y_name, metric = "rmse") {
  actual <- data_test[, y_name]
  predicted <- predict_y(model, data_test)
  return(list(error = error(actual, predicted, metric),
              below_error = percent_err_below(actual, predicted)))
}
