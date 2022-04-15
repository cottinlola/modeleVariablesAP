data_normalize <- function(data) {
  breeding <- c("bovins", "ovins_caprins", "polyélevage", "porcins",
                "volailles")
  farming <- c("arboriculture", "grandes_cultures", "maraichages",
               "polyculture", "viticulture")
  data <- reduce_by(data[data$TYPEXPL.IND %in% breeding, ], "UGB.IND")
  data <- reduce_by(data[data$TYPEXPL.IND %in% farming, ],
                    "SURFACE.IND")
}