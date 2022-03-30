#' Calcul du VIF (pour étudier la multi-colinéarité) et graphique
#' Si VIF proche de 1 : modèle + robuste
#' car les facteurs ne sont pas influencés par la corrélation avec d'autres facteurs
#'
#' @param mod le modèle
#' @param graph un bouléen TRUE pour tracer le graphique du VIF ou FALSE sinon
#' @return numeric ou plot : retourne les valeurs du VIF pour chaque variable explicative ou le graphique
#'
#' @export
#'
#' @examples
#' vif(modele_lineaire, graph = TRUE)

ind_vif <- function(mod, graph = FALSE){
  car::vif_values <- vif(mod)
  if (graph == TRUE){
    barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
    return(abline(v = 5, lwd = 3, lty = 2))
  }
  return(vif_values)
}
