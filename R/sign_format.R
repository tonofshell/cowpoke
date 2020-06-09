#' Format Significance Values
#'
#' An internal function for generating colors associated with significance values in hypothesis testing.
#'
#' @param x A numeric vector with significance values
#' @param breaks A numeric vector with values at which each color break occurs
#' @param values A character vector of colors to use for each interval
#' @param na_value A character vector of the color to use for NA values
#' @return A vector

sign_format = function(x, breaks = c(1, 0.1, 0.05, 0.01, 0.001, 0), values = c("#808080", "#ffa500", "#9acd32", "#008000", "#006400"), na_value = "#808080") {
  x = ifelse(is.na(x), na_value, x)
  for (i in 1:(length(breaks) - 2)) {
    x = ifelse(x <= breaks[i] & x > breaks[i + 1], values[i], x)
  }
  last_index = length(breaks)
  x = ifelse(x <= breaks[last_index - 1] & x >= breaks[last_index], values[last_index - 1], x)
  x
}
