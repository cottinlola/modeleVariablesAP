#' Returns a basic summary of a model's performance
#'
#' @param model The model to evaluate
#' @param data_test A data.frame of test data
#' @param y_name The variable to explain
#' @param metric The metric to use for error computation
#' @param est_below_cutoff "Acceptable" error in percent
#' @param err_above_cutoff Desired percent of "acceptable" estimated values
#' @return A data.frame holding the r.squared and errors metrics
#'
#' @export
#'
#' @examples
#' model_performance(model, data_test, "RESCO")
model_performance <- function(model, data_test, y_name, metric,
                              est_below_cutoff, err_above_cutoff) {
  r_squared <- summary(model)$r.squared
  if (is.null(r_squared)) {
    r_squared <- NA
  }
  err <- model_error(model, data_test, y_name, metric, est_below_cutoff,
                     err_above_cutoff)
  df <- data.frame(r = r_squared, err = err$err,
                   est_below = err$est_below, err_above = err$err_above)
  colnames(df) <- c("R2", metric, paste0("mape<=", est_below_cutoff, "%"),
                    paste0("mape.value=", err_above_cutoff, "%"))
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
                               est_below_cutoff, err_above_cutoff) {
  if (is.null(names)) {
    names <- names(models)
  }
  dfs <- lapply(models, function(model) model_performance(model, data_test,
                                                          y_name, metric,
                                                          est_below_cutoff,
                                                          err_above_cutoff))
  df <- do.call(rbind, dfs)
  rownames(df) <- names
  return(df)
}
