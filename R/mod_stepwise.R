#' Régression stepwise (pas-à-pas)
#'
#' @param X_names les noms des variables explicatives
#' @param y_name le nom de la variable à expliquer
#' @param data le jeu de données
#' @param direction la méthode utilisée dans le stepwise :
#'                  "backward", "forward" ou "both (par défaut)
#' @return model : le meilleur modèle (basé sur l'AIC) par la méthode stepwise
#'
#' @export
#'
#' @examples
#' mod_stepwise(data, X_names, y_name, "backward")
mod_stepwise <- function(data, X_names, y_name, direction = "both") {
  # Fit the full model
  fullmodel <- mod_lineaire(data, X_names, y_name)
  # Stepwise regression model
  stepmodel <- MASS::stepAIC(fullmodel, direction = direction, trace = FALSE)
  return(stepmodel)
}
