#' Recode Value(s) as Missing
#'
#' Appends a string to all selected variable names
#'
#' @param data_set A tibble/data frame
#' @return A tibble/data frame
#' @import tidyverse
#' @export

append_var_names = function(data_set, ..., prefix = "", suffix = "") {
  original_names = data_set %>% names()
  selected_variables = data_set %>% select(...) %>% names()
  if (length(selected_variables) == 0) {
    selected_variables = data_set %>% names()
  }
  new_names = original_names %>% length() %>% character()
  for (i in 1:length(original_names)) {
    if (original_names[i] %in% selected_variables) {
      new_names[i] = paste(prefix, original_names[i], suffix, sep = "")
    } else {
      new_names[i] = original_names[i]
    }
  }
  names(data_set) = new_names
  return(data_set)
}
