#' Robustness for sufficient terms (usually configurations)
#'
#' Evaluate robustness on the level of individual
#' sufficient terms (conditions or conjunctions/configurations) of models.
#' Aggregates over sufficient terms by calculating
#' their frequencies and the frequencies of terms co-occurring
#' in a model. The frequencies are plotted using Upset plots from
#' \pkg{UpSetR}.
#'
#' @importFrom magrittr %>%
#' @importFrom stringi stri_trim stri_unique
#' @importFrom purrr map
#' @import UpSetR
#'
#' @param ls List of QCA models as produced
#' with \code{\link[QCA]{minimize}} from \pkg{QCA}
#' package
#'
#' @param nsets Specifies number of sets to be plotted. Argument
#' imported from the \code{\link{upset}} function from \pkg{UpSetR}.
#'
#' @return A plot presenting the frequency of individual
#' terms and their co-occurrences across QCA models
#'
#' @examples
#' data("solutions_example")
#' configurations_robust(solutions_example)
#'
#' @export
configurations_robust <- function(ls, nsets = 5) {
  if (class(ls) != "list") {
    stop('The object is not a list. Function only processes lists.')
  }
  if (length(ls) == 1) {
    stop('One needs at least two QCA models to use the function.')
  }
  else {
    temp1 <- unlist(ls)
    temp1 <- purrr::map(temp1, function(x) stringi::stri_trim(x))
    all_values <- stringi::stri_unique(unlist(temp1))
    all_values
    finl <- detection(temp1, all_values)
    colnames(finl) <- all_values
    UpSetR::upset(finl, order.by = "freq", nsets = nsets)
  }
}

