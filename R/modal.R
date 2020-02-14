#' Mode of a Vector
#'
#' Returns the modal value of a vector
#'
#' @param vect a vector
#' @return A vector of the modal value(s)
#' @import tidyverse
#' @export

modal = function(vect) {
  crosstab =  vect %>% unlist() %>% table() %>% enframe()
  modals = crosstab %>% filter(value == max(value))
  if (is.numeric(vect)) {
    return(as.numeric(modals$name))
  }
  modals$name
}
