#' Prepare the dataset: convert to thousands when needed, restrict to
#' individuals present at least n_min_years, remove outliers and split in
#' train and test sets
#'
#' @param data A data.frame: the full dataset
#' @param reduce_mil A logical indicating if thousands conversion should be done
#' @param reduce_by A character specifying a variable to reduce by
#' @param n_min_years   An integer giving the minimal number of years a farm
#'                      should appeared in the data to be selected
#' @param outliers_custom_cutoff Threshold to identified outliers
#' @param split_pct_train Ratio between train and test data
#'
#' @return A list of data.frame: train and test sets
#'
#' @export
#'
#' @example
#' datasets <- data_prep_all(data)
#'
data_prep_all <- function(data, reduce_mil = FALSE, reduce_by = NULL,
                          n_min_years = 5, outliers_custom_cutoff = NULL,
                          split_pct_train = 0.9) {
  data <- data[, which(colSums(is.na(data)) == 0)]
  data <- selection_fermes(data, n_min_years)
  data <- supp_outliers(data, outliers_custom_cutoff)
  if (reduce_mil) {
    data <- conversion_milliers(data)
  }
  if (!is.null(reduce_by)) {
    data <- reduce_by(data, reduce_by)
  }
  data <- div_jeu(data, split_pct_train)
  return(data)
}