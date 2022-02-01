#' Robustness for single (INUS) conditions
#'
#' Evaluates robustness on the level of single conditions
#' or INUS conditions (Insufficient and Necessary conditions
#' that are part of a conjunction that is Unnecessary and
#' Sufficient). Aggregates over conditions by calculating
#' and plotting their frequencies and the frequencies of
#' conditions that co-occur in a QCA model
#'
#' @importFrom magrittr %>%
#' @importFrom stringi stri_split_fixed stri_unique
#' @importFrom purrr map
#' @importFrom plyr ldply
#' @import UpSetR
#'
#' @param ls List of QCA models as produced
#' with \code{\link[QCA-minimize]{QCA::minimize()}} from \pkg{QCA}
#' package
#' @param nsets Specifies number of sets to be plotted. Argument
#' imported from the \code{\link{upset}} function from \pkg{UpSetR}.
#'
#' @return A plot presenting the frequency of individual
#' conditions and their co-occurrences across QCA models.
#'
#' @examples
#' data("solutions_example")
#' conditions_robust(solutions_example)
#'
#' @export
conditions_robust <- function(ls, nsets = 5) {
  temp1 <- purrr::map(unlist(ls), function(x)
    stringi::stri_split_fixed(x, "*") %>% unlist())
  temp1 <- purrr::map(temp1, function(x)
    stringi::stri_split_fixed(x, "+") %>% unlist())
  all_values <- stringi::stri_unique(unlist(temp1))
  final_matrix <- plyr::ldply(temp1, function(y)
    comparison(x = all_values, y = y, num = T))
  final_matrix$.id <- NULL
  colnames(final_matrix) <- all_values
  UpSetR::upset(final_matrix, order.by = "freq", nsets = nsets)
}
