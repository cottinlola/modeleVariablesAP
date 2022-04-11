#' Reduces a data.frame numerical variables using another numerical variable
#'
#' @param data A data.frame: a dataset
#' @param reduce_by A character naming the variable to reduce by
#'
#' @return The reduced data.frame
#'
#' @export
#'
#' @example
#' data_r <- reduce_by(data, "SURFACE.IND")
#'
reduce_by <- function(data, reduce_by) {
  num_vars <- setdiff(get_numeric_var(data), c(reduce_by, "MILEX", "IDNUM"))
  data[, num_vars] <- data[, num_vars] / data[, reduce_by]
  return(data)
}