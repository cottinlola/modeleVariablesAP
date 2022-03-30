#' Entraine et retourne un modèle linéaire
#'
#' @param data le jeu de données
#' @param x_names les noms des variables explicatives
#' @param y_name le nom de la variable à expliquer
#' @return le modèle entrainé
#'
#' @importFrom stats as.formula lm coef median predict quantile sd
#' @importFrom graphics abline
#' @export
#'
#' @examples
#' mod_lineaire(data, "RESCO", c("MACHINE.IND", "MILEX"))
mod_lineaire <- function(data, x_names, y_name) {
  x_names <- setdiff(x_names, y_name)
  return(lm(as.formula(paste0(y_name, " ~ ",
                              paste0(x_names, collapse = " + "))), data = data))
}

#' Entraine des modèles linéaires simples et les retourne
#'
#' @param data_train le jeu de données d'entrainement
#' @param x_names les noms des variables explicatives
#' @param y_name le nom de la variable à expliquer
#' @return liste de modèles
#'
#' @export
#'
#' @examples
#' mod_uni_lineaires(data_train, "RESCO", c("MACHINE.IND", "MILEX"))
mod_uni_lineaires <- function(data_train, x_names, y_name) {
  x_names <- setdiff(x_names, y_name)
  mods <- lapply(x_names, function(x_name) {
    return(mod_lineaire(data_train, x_name, y_name))
  })
  rownames(mods) <- x_names
  return(mods)
}

#' Applique des modèles linéaires simples et renvoie le nom des variables
#' explicatives dont le modèle donne un R² supérieur à un seuil (défaut 70%)
#'
#' @param data le jeu de données
#' @param x_names les noms des variables explicatives
#' @param y_name le nom de la variable à expliquer
#' @param cutoff seuil à dépasser
#' @return vecteur :    nom des variables explicatives pour lesquelles le modèle
#'                      linéaire simple renvoie un R² supérieur à cutoff
#'
#' @export
#'
#' @examples
#' mod_lineaires_best_var(data, "RESCO", c("MACHINE.IND", "MILEX"))
mod_lineaires_best_var <- function(data, x_names, y_name, cutoff = .7) {
  x_names <- setdiff(x_names, y_name)
  r_squared <- sapply(x_names, function(x_name) {
    m <- mod_lineaire(data, x_name, y_name)
    return(summary(m)$r.squared)
  })
  return(x_names[r_squared >= cutoff])
}
