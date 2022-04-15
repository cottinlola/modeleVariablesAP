#' Normalize numerical data using the explotations' surface for farming kind
#' and the ugb for breeding ones.
#'
#' @param data A data.frame: the full dataset
#'
#' @return A data.frame normalized
#'
#' @export
#'
#' @example
#' data_norm <- data_normalize(data)
data_normalize <- function(data) {
  breeding <- c("bovins", "ovins_caprins", "polyélevage", "porcins",
                "volailles")
  farming <- c("arboriculture", "grandes_cultures", "maraichages",
               "polyculture", "viticulture")
  breeding_idx <- data$TYPEXPL.IND %in% breeding
  farming_idx <- data$TYPEXPL.IND %in% farming
  data[breeding_idx, ] <- reduce_by(data[breeding_idx, ], "UGB.IND")
  data[farming_idx, ] <- reduce_by(data[farming_idx, ], "SURFACE.IND")
  return(data)
}