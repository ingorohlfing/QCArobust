#' counting()
#'
#' Function that counts a specific string combination
#'
#' @import stringi
#' @param list the data from within which we want to count
#' @param terms are the terms that we need to count
#' @return returns a the count of terms within the list of strings
#' @note this functiion uses the stri_count function of the
#' stringi function to count  a pattern in a list of strings
#'
#' @export
counting <- function(list, terms) {
  temp <- stringi::stri_count_fixed(list, terms)
  final <- count(temp)
  return(final)
}
