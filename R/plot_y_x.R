#' Plot y ~ x
#'
#' @param data le jeu de données
#' @param X_names les noms des variables explicatives
#' @param y_name le nom de la variable à expliquer
#' @return les graphiques y ~ x
#'
#' @export
#'
#' @examples
#' plot_y_x(data, "RESCO", c("MACHINE.IND", "MILEX"))
plot_y_x <- function(data, X_names, y_name) {
  if (length(X_names) > 1) {
    res <- sapply(X_names, function (X_name) plot_y_x(data, X_name, y_name))
  } else {
    res <- plot(as.formula(paste0(y_name, " ~ ",
                                  paste0(X_names, collapse = " + "))),
                data = data)
  }
  return(res)
}