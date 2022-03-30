#' Sélection des fermes présentes au moins X ans (par défaut 5 ans)
#'
#' @param data le jeu de données (avec la variable IDNUM)
#' @param nb_annees le nom de la variable à expliquer
#' @return dataframe : le jeu de données avec les fermes présentes
#'
#' @export
#'
#' @examples
#' selection_fermes(data_ind)

selection_fermes <- function(data, nb_annees = 5) {
  idnum <- table(data$IDNUM)
  data_ind_y <- data[data$IDNUM %in% names(idnum[idnum >= nb_annees]), ]
  return(data_ind_y)
}
