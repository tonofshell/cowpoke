#' Collapse to Other
#'
#' Collapases a vector into the n-1 most frequent categories with all other categories becoming "other"
#'
#' @param vect a vector
#' @param n_categories the total number of categories the returned vector will have
#' @param keep_numeric if keep_numeric is TRUE and the vector is numeric, the other category will be specified as a numeric value of repeated 8's that's greater than the maximum value instead of a character value
#' @return A vector
#' @import tidyverse
#' @export

#
collapse_to_other = function(vect, n_categories, keep_numeric = TRUE) {
  if (vect %>% unique() %>% length() < n_categories) {
    message("n_categories must be less than the number of unique values")
    return(vect)
  }
  var_table = vect %>% table() %>% as_tibble() %>% arrange(-n) %>% rename("val" = ".")
  other_cats = var_table$val[n_categories:length(var_table$val)]
  if (is.numeric(vect) && keep_numeric) {
    other_val = 8
    while (other_val < max(vect)) {
      other_val = other_val %>% paste0(8) %>% as.numeric()
    }
    vect[vect %in% other_cats] = other_val
  }
  was_factor = is.factor(vect)
  vect = vect %>% as.character()
  vect[vect %in% other_cats] = "other"
  if (was_factor) {
    return(factor(vect))
  }
  return(vect)
}
