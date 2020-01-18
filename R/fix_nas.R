#' Recode Value(s) as Missing
#'
#' Matches values from a vector of strings and recodes those values as NA
#'
#' @param vect a vector
#' @param na_strs a character vector of values you want to code as NA
#' @return A vector
#' @export

fix_nas = function(vect, na_strs) {
    vect[vect %in% na_strs] = NA
  return(vect)
}
