#' Graphique y^ en fonction de y
#'
#' @param model modèle
#' @param data_test données à estimer
#' @param y_name nom de la variable à expliquer du modèle
#' @return plot : le graphique y^ en fonction de y
#'
#' @import ggplot2
#' @export
#'
#' @examples
#'plot_y_hat(mod_en, data_test, "SUBEX")

plot_y_hat <- function(model, data_test, y_name) {
    df <- data.frame(y = data_test[, y_name],
                    y_hat = predict_y(model, data_test))
    ggplot(df, aes(x = y, y = y_hat)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour = "red") +
    theme_bw() + labs(x = "Observed", y = "Predicted")
}
