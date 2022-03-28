#' Applique un modèle mixte
#'
#' @param model modèle de prédiction
#' @param data_test jeu de test du modèle
#' @param allow_new_levels True par défaut
#' @return vecteur de prédiction de y
#' @examples
#' mod_mixtes(data_train, "RESCO", "(MILEX | IDNUM)")
predict_y <- function(model, data_test, allow_new_levels = T) {
    mymod <- class(model)[1]
    y_hat <- c()
    if (mymod %in% c("lm", "lmerMod")) {
        #Modèles linéaires simples/multiples, stepwise ou mixtes
        y_hat <- predict(model, newdata = data_test)
    }else if (mymod %in% c("cv.glmnet")) {
        #Modèles pénalisés
        vars <- setdiff(rownames(coef(model)), "(Intercept)")
        y_hat <- predict(model, newx = as.matrix(data_test[, vars]))
    }
    return(y_hat)
}