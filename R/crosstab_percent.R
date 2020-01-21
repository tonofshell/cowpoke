#' Calculate Percents for Contingency Tables
#'
#' Calculates percents for contingency tables (cross tabulations), by default selects the first n-2 variables (variable n is the number of observations, n-1 is the default omitted column)
#'
#' @param data_crosstab A contingency table or tibble/data frame of a contingency table
#' @param n_var The name of the variable which contains frequencies
#' @param vars A character vector of variable names
#' @param formatted If "TRUE", percents of frequencies will be a character vector with rounded percents and a percent sign (\%), otherwise the raw decimal integers will be returned
#' @return A contingency table as a tibble
#' @import tidyverse
#' @export

crosstab_percent = function(data_crosstab, n_var = "n", vars = c(), formatted = FALSE) {
  data_crosstab = data_crosstab %>% as_tibble(.name_repair = "minimal")
  len = length(data_crosstab[[1]])
  data_crosstab$percent = numeric(len)
  categories = vars
  if (length(categories) == 0) {
    categories = data_crosstab %>% select(-n_var, -percent) %>% names() %>% .[1:(length(.) -1)]
  }
  for (i in 1:len) {
    cats_subset = data_crosstab %>% select(-percent)
    for (category in categories) {
      formatted_cat = category
      if (str_detect(formatted_cat, " ")) {
        formatted_cat = paste("`", formatted_cat, "`", sep = "")
      }
      cats_subset = cats_subset %>% filter(eval(parse(text = formatted_cat)) == data_crosstab[[category]][i])
    }
    perc = data_crosstab[[n_var]][i] / sum(cats_subset[[n_var]])
    if (formatted) {
      perc = paste(round(perc * 100, 1), "%", sep = "")
    }
    data_crosstab$percent[i] = perc
  }
  return(data_crosstab)
}
