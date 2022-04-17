#' Applique un modèle mixte
#'
#' @param data jeu d'apprentissage du modèle
#' @param x_names vecteur des noms des variables explicatives
#' @param y_name variable à expliquer
#' @param effets effets aléatoires
#' @param formule formule de l'équation du modèle
#'                (par défaut : y_name ~ vars_1 + vars_2 + ... + effets)
#' @return model
#'
#' @export
#'
#' @examples
#' mod_mixtes(data_train, "RESCO", "(MILEX | IDNUM)")
mod_mixtes <- function(data, x_names, y_name, effets, formule = "auto") {
    if (formule == "auto") {
        formule <- as.formula(paste0(y_name, "~",
                                     paste0(c(x_names, effets), collapse = "+")))
    }
    mod_lmer <- lme4::lmer(formule, data = data,
                           control =
                             lme4::lmerControl(
                               optimizer = "bobyqa",
                               optCtrl = list(maxfun = 2e5))
)
    return(mod_lmer)
}