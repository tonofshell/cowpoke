#' Format Number
#'
#' Formats numeric vectors, rounding values to a reasonable amount of digits, and inserting separators for large numbers. Useful for inserting inline numeric values in R Markdown.
#'
#' @param x A numeric vector
#' @param round_digits The number of digits to round numbers at each cutoff (< 1, < 10, < 100, >= 100). Default value is 3, 2, 1, 0.
#' @param large_num_sep A character to separate every three digits in large numbers. Default value is the American standard of using a comma (",").
#' @return A character vector
#' @examples format_number(pi * c(0.1, 1, 10, 100, 1000))
#' "0.314" "3.14"  "31.4"  "314"   "3,142"
#' @export

#
format_number = function(x, round_digits = c(3, 2, 1, 0), large_num_sep = ",") {
  rounder = function(z) {
    if (z < 1) {
      return(as.character(round(z, round_digits[1])))
    }
    if (z < 10) {
      return(as.character(round(z, round_digits[2])))
    }
    if (z < 100) {
      return(as.character(round(z, round_digits[3])))
    }
    round(z, round_digits[4]) %>% formatC(format = "d", big.mark = large_num_sep)
  }
  sapply(x, rounder)
}
