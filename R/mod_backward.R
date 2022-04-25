#' Backward regression based on adjusted R2
#'
#' @param data training dataset
#' @param x_names X variables
#' @param y_name y variable
#' @return best (ie parsimonious) model
#'
#' @export
#'
#' @examples
#' mod_backward(data, x_names, y_name")
mod_backward <- function(data, x_names, y_name, r2_threshold = .01) {
  # only try to remove numeric var
  x_num_names <- get_numeric_var(data[, x_names])
  x_non_num_names <- setdiff(x_names, x_num_names)
  x_names <- x_num_names
  # fit the full model
  best_mod_sum <- summary(mod_lineaire(data, x_names, y_name))
  removed_vars <- NULL
  while (TRUE) {
    # get adujsted R2
    adj_r2 <- best_mod_sum$adj.r.squared
    coefs <- best_mod_sum$coefficients
    # pval (ignoring intercept)
    coefs_pval <- coefs[2:nrow(coefs), 4]
    # variables by highest p.val not already tested for removal
    x_to_remove <- setdiff(names(coefs_pval[order(coefs_pval,
                                                  decreasing = TRUE)]),
                           removed_vars)
    if (length(x_to_remove) == 0) {
      # no more variable to test for removal
      break
    }
    x_to_remove <- x_to_remove[[1]]
    removed_vars <- c(removed_vars, x_to_remove)
    res <- test_for_removal(best_mod_sum, data, x_names, y_name, x_to_remove,
                            r2_threshold)
    best_mod_sum <- res$best_mod_sum
    x_names <- res$x_names
  }
  # add back non numeric vars
  x_names <- c(x_names, x_non_num_names)
  for (x_name in x_non_num_names) {
    res <- test_for_removal(best_mod_sum, data, x_names, y_name, x_name,
                            r2_threshold)
    best_mod_sum <- res$best_mod_sum
    x_names <- res$x_names
  }
  return(list(model = mod_lineaire(data, x_names, y_name), x_names = x_names))
}

test_for_removal <- function(best_mod_sum, data, x_names, y_name, x_to_remove,
                             r2_threshold) {
  # get adujsted R2
  adj_r2 <- best_mod_sum$adj.r.squared
  # remove variable
  x_names <- setdiff(x_names, x_to_remove)
  model <- mod_lineaire(data, x_names, y_name)
  mod_sum <- summary(model)
  if (adj_r2 - mod_sum$adj.r.squared < r2_threshold) {
    best_mod_sum <- mod_sum
  } else {
    # put back variable
    x_names <- c(x_names, x_to_remove)
  }
  return(list(best_mod_sum = best_mod_sum, x_names = x_names))
}
