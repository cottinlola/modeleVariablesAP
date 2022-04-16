#' Applique un modèle mixte
#'
#' @param data jeu d'apprentissage du modèle
#' @param y_name variable à expliquer
#' @param effets effets aléatoires
#' @param vars vecteur des noms des variables explicatives
#'             (par défaut toutes les variables sauf y)
#' @param formule formule de l'équation du modèle
#'                (par défaut : y_name ~ vars_1 + vars_2 + ... + effets)
#' @return model
#'
#' @export
#'
#' @examples
#' mod_mixtes(data_train, "RESCO", "(MILEX | IDNUM)")
mod_mixtes <- function(data, y_name, effets,
                       vars = setdiff(names(data), y_name),
                       formule = "auto") {
    if (formule == "auto") {
        formule <- as.formula(paste0(y_name, "~",
                                     paste0(c(vars, effets), collapse = "+")))
    }
    mod_lmer <- lme4::lmer(formule, data = data,
                           control =
                             lme4::lmerControl(
                               optimizer = "bobyqa",
                               optCtrl = list(maxfun = 2e5),
                               boundary.tol = 1e-2,
                               check.conv.singular =
                                 lme4::.makeCC(action = "ignore", tol = 1e-2),
                               tolPwrss = 1e-2)
)
    return(mod_lmer)
}