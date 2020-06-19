#' Counts frequency of a term in list of results
#'
#' @importFrom sjmisc stri_find
#'
#' @param list List of terms that is searched for a match
#' @param term Term (single condition or conjunction) that should
#' be searched for
#'
#' @return Count of search terms in the list that has been searched
#'
#' @examples
#' conjunction_list <- list(c("ABC", "ACD", "ABD"))
#' counting(conjunction_list, "AC")
#' counting(conjunction_list, "AB")
#'
#' @export
counting <- function(list, term) {
  temp <- sjmisc::stri_find(list, term, precision = 2, partial = 2)
  final <- count(temp)
  names(final)[1] <- term
  return(final)
}
