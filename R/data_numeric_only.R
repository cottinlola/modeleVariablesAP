#' Restricts data.frame variables to numerical only
#'
#' @param data A data.frame: the full dataset
#'
#' @return The data.frame with only it's numerical columns
#'
#' @export
#'
#' @example
#' data_num <- data_numeric_only(data)
#'
data_numeric_only <- function(data) {
  return(data[, get_numeric_var(data)])
}

get_numeric_var <- function(data) {
  return(names(which(unlist(lapply(data, is.numeric)))))
}