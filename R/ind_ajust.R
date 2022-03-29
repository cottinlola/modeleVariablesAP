#' Calcul de l'écart quadratique moyen (RMSE)
#'
#' @param actual le vecteur des "vraies" valeurs
#' @param predicted le vecteur des valeurs estimées
#' @return numeric : l'écart quadratique moyen (RMSE)
#' @examples
#' rmse(actual = c(35, 36), predicted = c(36, 34))

rmse <- function(actual, predicted){
  return(sqrt(mean((actual - predicted)^2)))
}
