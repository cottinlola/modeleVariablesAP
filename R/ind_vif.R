#' Calcul du VIF (pour étudier la multi-colinéarité) et graphique
#' Si VIF proche de 1 : modèle + robuste
#' car les facteurs ne sont pas influencés par
#' la corrélation avec d'autres facteurs
#'
#' @param mod le modèle
#' @param graph un bouléen TRUE pour tracer le graphique du VIF ou FALSE sinon
#' @return numeric ou plot : retourne les valeurs du VIF
#'          pour chaque variable explicative ou le graphique
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' vif(modele_lineaire, graph = TRUE)
ind_vif <- function(mod, graph = FALSE) {
  vif_values <- car::vif(mod)
  if (graph == TRUE) {
    df <- vif_to_df(vif_values)
    p <- ggplot(df, aes(y = var_names, x = value)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() + labs(y = "", x = "VIF values")
    return(p)
  }
  return(vif_values)
}

#' Retourne un data.frame à partir d'un objet retourné par car::vif
#'
#' @param obj l'objet retourné par car::vif
#' @return un data.frame donnant le vif de chaque variable
#'
#' @export
#'
#' @examples
#' vif_to_df(vif_values)
vif_to_df <- function(obj, ...) UseMethod("vif_to_df")

vif_to_df.matrix <- function(obj) {
  return(data.frame(var_names = rownames(obj), value = obj[, "GVIF"]))
}

vif_to_df.numeric <- function(obj) {
  return(data.frame(var_names = names(obj), value = obj))
}
