#' Divise le jeu de données en jeu d'apprentissage et en jeu de test
#' 
#' @param data le jeu de données
#' @param pct_train ratio pour le jeu d'apprentissage
#' @return liste : jeu d'apprentissage , jeu de test
#' @examples
#' div_jeu(data)
#' div_jeu(data, 0.7)
div_jeu <- function(data, pct_train = 0.9) {
    idx_train <- sample(nrow(data), round(nrow(data) * 0.9))
    data_train <- data[idx_train, ]
    data_test <- data[-idx_train, ]
    data_div <- list(train = data_train, test = data_test)
    return(data_div)
}