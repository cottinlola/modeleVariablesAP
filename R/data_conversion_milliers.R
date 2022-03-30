#' Conversion en milliers d'euros
#'
#' @param data le jeu de données
#' @return dataframe : le jeu de données avec les données converties en milliers d'euros
#'
#' @export
#'
#' @examples
#' conversion_milliers(data)

conversion_milliers <- function(data){
  var_num <- setdiff(names(which(sapply(data, is.numeric))), c("IDNUM", "MILEX"))
  medians <- sapply(data[, var_num], median)
  var_milliers <- names(which(medians >= 1000))
  data_ind_milliers <- data
  data_ind_milliers[, var_milliers] <- data_ind_milliers[, var_milliers] / 1000
  return(data_ind_milliers)
}
