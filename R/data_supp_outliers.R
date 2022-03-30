#' Suppression des outliers
#'
#' @param data le jeu de données
#' @param custom_cutoff permet de spécifier un cutoff arbitraire
#' @param graph pour représenter les points (TRUE ou FALSE)
#' @return dataframe ou plot : le jeu de données sans
#'         les outliers ou la représentation graphique
#'
#' @export
#'
#' @examples
#' supp_outliers(data_ind_5y, graph = TRUE)

supp_outliers <- function(data, custom_cutoff = NULL, graph = FALSE) {
  var_num <- setdiff(names(which(sapply(data, is.numeric))),
              c("IDNUM", "MILEX"))
  data_num <- data[, var_num]
  # 0n garde uniquement les variables avec suffisamment de variabilité
  t <- sapply(data_num, function(c) {
    tt <- table(c)
    max(tt) / length(tt) * 100
  })
  data_num <- data_num[, names(data_num) %in% names(which(t < 50))]
  # On se base sur la distance de Mahalanobis
  outliers <- mvoutlier::sign1(as.matrix(data_num))
  cut_off <- custom_cutoff
  if (is.null(cut_off)) {
    cut_off <- quantile(outliers$x.dist, .75) + 3 * iq
  }
  data_no_outliers <- data[which(outliers$x.dist <= cut_off), ]
  # plot outliers
  if (graph == TRUE) {
    plot(outliers$x.dist)
    return(abline(a = cut_off, b = 0, col = "red"))
  }
  return(data_no_outliers)
}
