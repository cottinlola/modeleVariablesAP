#' Main function
#'
#' @param data A data.frame: the full dataset
#' @param conv_mil A logical indicating if thousands conversion should be done
#' @param n_min_years   An integer giving the minimal number of years a farm
#'                      should appeared in the data to be selected
#' @param outliers_custom_cutoff Threshold to identified outliers
#' @param split_pct_train Ratio between train and test data
#' @param remove_non_num    A logicial indicating if non numeric columns should
#'                          be ignored
#' @param data_trian A data.frame: the training dataset
#' @param data_test A data.frame: the testing dataset
#' @param x_names A character vector: explainatory variables
#' @param x_exclude A character vector: explainatory variables to exclude
#' @param y_name A character: the variable to explain
#'
#' @return
#'
#' @export
#'
#' @example
#' res <- run(data = data, y_name = "SUBEX")
#' res <- run(data = data, x_names = "MACHINE.IND", y_name = "SUBEX")
#' res <- run(data = data, x_exclude = c("RESCO", "..."), y_name = "SUBEX")
#' res <- run(data_train = data_train, data_test = data_test, y_name = "SUBEX")
#'
run <- function(data = NULL, conv_mil = FALSE, n_min_years = 5,
                outliers_custom_cutoff = NULL, split_pct_train = 0.9,
                remove_non_num = TRUE,
                data_train = NULL, data_test = NULL,
                x_names = NULL, x_exclude = NULL, y_name) {
  if (is.null(data_train) | is.null(data_test)) {
    datasets <- data_prep_all(data, conv_mil, n_min_years,
                              outliers_custom_cutoff, split_pct_train,
                              remove_non_num)
    data_train <- datasets$train
    data_test <- datasets$test
  }
  if (is.null(x_names)) {
    x_names <- colnames(data_train)
  }
  x_names <- setdiff(x_names, c(x_exclude, c("IDNUM", "MILEX"), y_name))

  # TODO: add models, etc...
}