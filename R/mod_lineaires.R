#' Applique des modèles linéaires simples et renvoie le nom des variables explicatives dont le modèle donne un R² supérieur à 70%
#'
#' @param x_name les noms des variables explicatives
#' @param y_name le nom de la variable à expliquer
#' @param data le jeu de données
#' @return vecteur : nom des variables explicatives pour lesquelles le modèle linéaire simple renvoie un R² supérieur à 70%
#'
#' @export
#'
#' @examples
#' mod_lineaires("RESCO", c("MACHINE.IND", "MILEX"), data)

mod_lineaires <- function(x_name, y_name, data){
  x_select <- c()
  for (i in 1:length(x_name)){
    m <- lm(as.formula(paste0(y_name, " ~ ", x_name[i])), data = data)
    if (summary(m)$r.squared >= 0.07){
      x_select <- c(x_select, x_name[i])
    }
  }
  return(x_select)
}
