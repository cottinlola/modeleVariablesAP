#' Renvoie les y prédits/estimés d'un modèle
#'
#' @param model modèle de prédiction
#' @param data_test jeu de test du modèle
#' @param allow_new_levels True par défaut
#' @return vecteur de prédiction de y
#'
#' @export
#'
#' @examples
#' predict_y(mod_en, data_test)
#' predict_y(mod_lm, data_test)
#' predict_y(mod_sw, data_test)
#' predict_y(mod_lmer, data_test)
#' predict_y(mod_lmer, data_test, allow_new_levels = F)
predict_y <- function(model, data_test, allow_new_levels = T) {
    mymod <- class(model)[1]
    y_hat <- NULL
    if (mymod %in% c("lm", "lmerMod")) {
        #Modèles linéaires simples/multiples, stepwise ou mixtes
        y_hat <- predict(model, newdata = data_test,
                         allow_new_levels = allow_new_levels)
    } else if (mymod %in% c("elnet", "cv.glmnet", "glmnet")) {
        #Modèles pénalisés
        vars <- setdiff(rownames(coef(model)), "(Intercept)")
        y_hat <- as.vector(predict(model, newx = as.matrix(data_test[, vars])))
    }
    return(y_hat)
}