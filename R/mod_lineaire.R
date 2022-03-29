#' Entraine et retourne un modèle linéaire
#'
#' @param data le jeu de données
#' @param X_names les noms des variables explicatives
#' @param y_name le nom de la variable à expliquer
#' @return le modèle entrainé
#'
#' @export
#'
#' @examples
#' mod_lineaire(data, "RESCO", c("MACHINE.IND", "MILEX"))
mod_lineaire <- function(data, X_names, y_name) {
  return(lm(as.formula(paste0(y_name, " ~ ",
                              paste0(X_names, collapse = " + "))), data = data))
}

#' Applique des modèles linéaires simples et renvoie le nom des variables
#' explicatives dont le modèle donne un R² supérieur à un seuil (défaut 70%)
#'
#' @param data le jeu de données
#' @param X_names les noms des variables explicatives
#' @param y_name le nom de la variable à expliquer
#' @param cutoff seuil à dépasser
#' @return vecteur :    nom des variables explicatives pour lesquelles le modèle
#'                      linéaire simple renvoie un R² supérieur à cutoff
#'
#' @export
#'
#' @examples
#' mod_lineaires(data, "RESCO", c("MACHINE.IND", "MILEX"))
mod_lineaires_best_var <- function(data, X_names, y_name, cutoff = .7) {
  r.squared <- sapply(X_names, function (X_name) {
    m <- mod_lineaire(data, X_name, y_name)
    return(summary(m)$r.squared)
  })
  return(X_names[r.squared >= cutoff])
}
