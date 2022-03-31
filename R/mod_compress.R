#' Compress a model in size by removing attributes not needed for prediction
#'
#' @param model Model to reduce
#'
#' @return A compress model
#'
#' @export
#'
#' @examples
#' mod_compress(model)
mod_compress <- function(model) {
  for (attr in c("y", "model", "residuals", "fitted.values", "effects", "linear.predictors", "weights",
                 "prior.weights", "data")) {
    if (attr %in% names(model)) {
      model[[attr]] <- NULL
    }
  }

  if ("qr" %in% names(model)) {
    if ("qr" %in% model$qr) {
      model$qr$qr <- NULL
    }
  }

  for (attr in c("terms", "formula")) {
    if (attr %in% names(model)) {
      attr(model[[attr]], ".Environment") <- NULL
    }
  }

  if ("family" %in% names(model)) {
    for (attr in c("variance", "dev.resids", "validmu", "simulate")) {
      model$family[[attr]] <- NULL
    }
  }

  return(model)
}