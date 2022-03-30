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
#' @export
#'
#' @examples
#' vif(modele_lineaire, graph = TRUE)

ind_vif <- function(mod, graph = FALSE) {
  vif_values <- car::vif(mod)
  if (graph == TRUE) {
    df <- data.frame(var_names = names(vif_values), value = vif_values)
    p <- ggplot2::ggplot(df, aes(y = var_names, x = value)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::theme_minimal() + ggplot2::labs(y = "", x = "VIF values")
    return(p)
  }
  return(vif_values)
}
