#' Returns a basic summary of a model's performance
#'
#' @param model The model to evaluate
#' @param data_test A data.frame of test data
#' @param y_name The variable to explain
#' @param metric The metric to use for error computation
#' @param below_cutoff seuil à dépasser
#' @return A data.frame holding the r.squared and errors metrics
#'
#' @export
#'
#' @examples
#' model_performance(model, data_test, "RESCO", c("MACHINE.IND", "MILEX"))
model_performance <- function(model, data_test, y_name, metric, below_cutoff) {
  r_squared <- summary(model)$r.squared
  err <- model_error(model, data_test, y_name, metric, below_cutoff)
  df <- data.frame(r = round(r_squared, digits = 2), err = err$error,
                   below_error = paste0(formatC(err$below_error, digits = 2,
                                                format = "f"), "%"))
  colnames(df) <- c("R2", metric, paste0("mape<=", below_cutoff, "%"))
  return(df)
}

#' Returns a basic summary of models' performances
#'
#' @param models The models to evaluate
#' @param names The models' names
#' @param data_test A data.frame of test data
#' @param y_name The variable to explain
#' @param metric The metric to use for error computation
#' @param below_cutoff seuil à dépasser
#' @return A data.frame holding the r.squared and errors metrics
#'
#' @export
#'
#' @examples
#' models_performance(models, names, data_test, "RESCO",
#'                    c("MACHINE.IND", "MILEX"))
models_performance <- function(models, names = NULL, data_test, y_name, metric,
                               below_cutoff) {
  if (is.null(names)) {
    names <- names(models)
  }
  dfs <- lapply(models, function(model) model_performance(model, data_test,
                                                          y_name, metric,
                                                          below_cutoff))
  df <- do.call(rbind, dfs)
  rownames(df) <- names
  return(df)
}