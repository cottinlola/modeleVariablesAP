#' Plot y ~ x
#'
#' @param data le jeu de données
#' @param x_names les noms des variables explicatives
#' @param y_name le nom de la variable à expliquer
#' @return les graphiques y ~ x
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' plot_y_x(data, "RESCO", c("MACHINE.IND", "MILEX"))
plot_y_x <- function(data, x_names = NULL, y_name) {
  if (is.null(x_names)) {
    x_names <- setdiff(colnames(data), y_name)
  }

  if (length(x_names) > 1) {
    figs <- lapply(x_names, function(x_name) plot_y_x(data, x_name, y_name))
  } else {
    figs <- ggplot(data.frame(y = data[, y_name], x = data[, x_names]),
                   aes(x = x, y = y)) +
      geom_point() +
      theme_bw() + labs(x = x_names, y = y_name)
  }
  return(figs)
}