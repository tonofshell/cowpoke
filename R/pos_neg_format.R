#' Format Magnitude of Values
#'
#' An internal function for generating colors associated with how close a value is to the absolute maximum/minimum value in the vector
#'
#' @param x A numeric vector
#' @param values A character vector of colors to use for each interval
#' @return A vector


pos_neg_format = function(x, values = c("black", "grey35", "black")) {
  std_x = (x / (2 * max(abs(x)))) + 0.5
  colors = colorRamp(values)(std_x) %>% (function(x) x / 255) %>% {rgb(.[,1], .[,2], .[,3])}
  colors
}
