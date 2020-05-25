#' Counts frequency of a specific string combination
#'
#' @importFrom stringi stri_count_fixed
#'
#' @param list List of terms that is searched for a match
#' @param terms Terms (single condition or conjunction) that should
#' be searched for
#'
#' @return Count of search terms in the list that has been searched
#'
#' @examples
#' conjunction_list <- list(c("ABC", "ACD", "ADF"))
#' counting(conjunction_list, "AC")
#'
#' @export
counting <- function(list, terms) {
  temp <- stringi::stri_count_fixed(list, terms)
  final <- count(temp)
  return(final)
}
