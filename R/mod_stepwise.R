#' Régression stepwise (pas-à-pas)
#'
#' @param x_name les noms des variables explicatives
#' @param y_name le nom de la variable à expliquer
#' @param data le jeu de données
#' @param direction la méthode utilisée dans le stepwise :
#'                  "backward", "forward" ou "both (par défaut)
#' @return model : le meilleur modèle (basé sur l'AIC) par la méthode stepwise
#' @examples
#' mod_stepwise(x_name, y_name, data_train, "backward")

mod_stepwise <- function(x_name, y_name, data, direction = "both") {
  # Fit the full model
  fullmodel <- lm(as.formula(paste0(y_name, " ~ ",
                             paste0(x_name, collapse = " + "))),
                             data = data)
  # Stepwise regression model
  stepmodel <- MASS::stepAIC(fullmodel, direction = direction, trace = FALSE)
  return(stepmodel)
}
