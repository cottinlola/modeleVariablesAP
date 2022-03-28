#' Suppression des outliers
#'
#' @param data le jeu de données
#' @param graph pour représenter les points (TRUE ou FALSE)
#' @return dataframe ou plot : le jeu de données sans les outliers ou la représentation graphique
#' @examples
#' supp_outliers(data_ind_5y, TRUE)

supp_outliers <- function(data, graph = FALSE){
  var_num <- setdiff(names(which(sapply(data, is.numeric))), c("IDNUM", "MILEX"))
  data_ind_num <- data[, var_num]
  # Pn garde uniquement les variables avec suffisamment de variabilité
  t <- sapply(data_ind_num, function(c) {
    tt <- table(c)
    max(tt) / length(tt) * 100
  })
  data_ind_num <- data_ind_num[, names(data_ind_num) %in% names(which(t < 50))]
  # On se base sur la distance de Mahalanobis
  outliers <- sign1(as.matrix(data_ind_num), qcrit = .995)
  cut_off <- outliers$const
  data_ind_no_outliers <- data_ind_5y[which(outliers$x.dist <= cut_off), ]
  # plot outliers
  if (graph == TRUE){
    plot(outliers$x.dist)
    return(abline(a = cut_off, b = 0, col = "red"))
  }
  return(data_ind_no_outliers)
}
