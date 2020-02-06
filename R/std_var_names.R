#' Standardize Variable Names
#'
#' Formats variable names, replacing all spaces with underscores, and making all letters lowercase
#'
#' @param data_set A tibble/data frame
#' @return A tibble/data frame
#' @import tidyverse
#' @export

#
std_var_names = function(data_set) {
  names(data_set) = data_set %>% names() %>% str_replace_all(" ", "_") %>% str_to_lower()
  return(data_set)
}
