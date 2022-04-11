#' Main function
#'
#' @param data A data.frame: the full dataset
#' @param reduce_mil A logical indicating if thousands conversion should be done
#' @param reduce_by A character specifying a variable to reduce by
#' @param n_min_years   An integer giving the minimal number of years a farm
#'                      should appeared in the data to be selected
#' @param outliers_custom_cutoff Threshold to identified outliers
#' @param split_pct_train Ratio between train and test data
#' @param data_train A data.frame: the training dataset
#' @param data_test A data.frame: the testing dataset
#' @param x_names A character vector: explainatory variables
#' @param x_exclude A character vector: explainatory variables to exclude
#' @param x_exclude_non_num A logicial indicating if non numeric columns should
#'                          be ignored
#' @param effects A character of random effects to consider
#' @param y_name A character: the variable to explain
#' @param metric A character: the metric to use for error computation
#' @param est_below_cutoff "Acceptable" error in percent
#' @param err_above_cutoff Desired percent of "acceptable" estimated values
#' @param traces A logical indicating if logs should be printed
#'
#' @return A list of models, their performances and theirs variables
#'
#' @export
#'
#' @example
#' res <- run(data = data, y_name = "SUBEX")
#' res <- run(data = data, x_names = "MACHINE.IND", y_name = "SUBEX")
#' res <- run(data = data, x_exclude = c("RESCO", "..."), y_name = "SUBEX")
#' res <- run(data_train = data_train, data_test = data_test, y_name = "SUBEX")
#'
run <- function(data = NULL, reduce_mil = FALSE, reduce_by = NULL,
                n_min_years = 5, outliers_custom_cutoff = NULL,
                split_pct_train = 0.9,
                data_train = NULL, data_test = NULL,
                x_names = NULL, x_exclude = NULL, x_exclude_non_num = FALSE,
                effects = "(MILEX | IDNUM)",
                y_name,
                metric = "rmse", est_below_cutoff = 10, err_above_cutoff = 80,
                traces = FALSE) {
  if (is.null(data_train) | is.null(data_test)) {
    datasets <- data_prep_all(data, reduce_mil = reduce_mil,
                              reduce_by = reduce_by, n_min_years = n_min_years,
                              outliers_custom_cutoff = outliers_custom_cutoff,
                              split_pct_train = split_pct_train)
    data_train <- datasets$train
    data_test <- datasets$test
  }
  if (is.null(x_names)) {
    x_names <- colnames(data_train)
  }
  x_names <- setdiff(x_names, c(x_exclude, "IDNUM", y_name))
  if (x_exclude_non_num) {
    x_names <- get_numeric_var(data_train[, x_names])
  }

  # Linear
  if (traces) print("Linear")
  # Univariate
  if (traces) print("Univariate")
  mods_uni_lm <- mod_uni_lineaires(data_train, x_names, y_name)
  # Multivariate
  if (traces) print("Multivariate")
  mod_all_lm <- mod_lineaire(data_train, x_names, y_name)
  # EN
  if (traces) print("EN")
  mod_en <- mod_cv_penalized(data_train, x_names, y_name)
  x_en <- model_variables(mod_en)
  mod_en_lm <- mod_lineaire(data_train, x_en, y_name)
  # Stepwise
  if (traces) print("Stepwise")
  mod_step_lm <- mod_stepwise(data_train, x_names, y_name)
  x_step <- model_variables(mod_step_lm)

  # Mixte
  if (traces) print("Mixte")
  if (traces) print("Multivariate")
  mod_all_mxt <- mod_mixtes(data_train, y_name, effects, x_names)
  if (traces) print("EN")
  mod_en_mxt <- mod_mixtes(data_train, y_name, effects, x_en)
  if (traces) print("Stepwise")
  mod_step_mxt <- mod_mixtes(data_train, y_name, effects, x_step)

  models <- c(mods_uni_lm, list(mod_all_lm), list(mod_en_lm), list(mod_step_lm),
              list(mod_all_mxt), list(mod_en_mxt), list(mod_step_mxt))
  names(models) <- c(names(mods_uni_lm), "all_lm", "en_lm", "step_lm",
                     "all_mxt", "en_mxt", "step_mxt")
  if (traces) print("Performances")
  models_perf <- models_performance(models = models, data_test = data_test,
                                    y_name = y_name, metric = metric,
                                    est_below_cutoff = est_below_cutoff,
                                    err_above_cutoff = err_above_cutoff)
  if (traces) print("Variables")
  models_vars <- models_variables(models = models)

  return(list(models = models, perf = models_perf, vars = models_vars))
}