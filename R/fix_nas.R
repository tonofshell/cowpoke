#' Recode Value(s) as Missing
#'
#' Matches values from a vector of strings and recodes those values as NA
#'
#' @param vect a vector
#' @param na_strs a character vector of values you want to code as NA
#' @param ignore_case a logical vector indicating whether case should be ignored when matching values to replace with NA
#' @return A vector
#' @export

fix_nas = function(vect, na_strs, ignore_case = FALSE) {
  if (ignore_case && !is.numeric(vect)) {
    vect[str_to_lower(vect) %in% str_to_lower(na_strs)] = NA
  } else {
    vect[vect %in% na_strs] = NA
  }
  vect
}
