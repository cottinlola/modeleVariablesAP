#' Calcul de l'écart quadratique moyen (RMSE)
#'
#' @param actual le vecteur des "vraies" valeurs
#' @param predicted le vecteur des valeurs estimées
#' @return numeric : l'écart quadratique moyen (RMSE)
#' @examples
#' rmse(actual = c(35, 36), predicted = c(36, 34))
rmse <- function(actual, predicted) {
  return(sqrt(mean((actual - predicted)^2)))
}

#' Calcul de l'écart moyen absolu (MAE)
#'
#' @param actual le vecteur des "vraies" valeurs
#' @param predicted le vecteur des valeurs estimées
#' @return numeric : l'écart moyen absolu (MAE)
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
#' @examples
#' get_metric_fun("mae")
#' get_metric_fun("rmse")
#' get_metric_fun("mape")
get_metric_fun <- function(metric) {
  fun <- NULL
  if (metric == "mae") {
    fun <- mae
  } else if (metric == "rmse") {
    fun <- rmse
  } else if (metric == "mape") {
    fun <- mape
  }
  return(fun)
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
#' @examples
#' percent_err_below(actual = c(35, 36), predicted = c(36, 34), cutoff = 5,
#'                   metric = "mape")
percent_err_below <- function(actual, predicted, cutoff, metric = "mape") {
  fun <- get_metric_fun(metric)
  errs <- mapply(fun, actual, predicted)
  return(100 * sum(errs <= cutoff) / length(actual))
}
