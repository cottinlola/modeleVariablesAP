#' Applique un modèle mixte
#'
#' @param data_train jeu d'apprentissage du modèle
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
mod_mixtes <- function(data_train, y_name, effets,
                        vars = setdiff(names(data_train), y_name),
                        formule = "auto") {
    if (formule == "auto") {
        formule <- as.formula(paste0(y_name, "~",
                                paste0(c(vars, effets), collapse = "+")))
    }
    mod_lmer <- lme4::lmer(formule, data = data_train)
    return(mod_lmer)
}