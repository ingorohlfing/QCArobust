#' Function decomposes results into individual sufficient
#' terms and creates intersection plots.
#'
#' @importFrom magrittr %>%
#' @importFrom stringi stri_trim stri_unique
#' @importFrom purrr map
#' @import UpSetR
#'
#' @param df Dataframe of QCA solutions as produced
#' with \code{\link[QCA]{minimize}} from \code{\link{QCA}}
#' package
#' @param const Choose between the plotting of all solutions
#' (\code{const = F} or only those meeting a minimum consistency
#' threshold specified with \code{const}.
#' @param y is an argument with default value of zero. If
#' any other number is set for the given argument, only
#' those configurations which are above the set value for y
#' will be plotted by the function
#' @param nsets Specifies number of sets to be plotted. Argument
#' imported from the \code{\link{upset}} function from \pkg{UpSetR}.
#'
#' @return A plot presenting the frequency of individual
#' conditions and their co-occurrences across QCA solutions.
#'
#' @export
config_upset <- function(df, const = FALSE, nsets) {

  if (!const) {
    temp1 <- unlist(df$solution)

  }
  else {
    temp1 <- cons_minimum(df)
    temp1 <- purrr::map(temp1$config, unlist)
  }
  temp1 <- purrr::map(temp1, function(x) stringi::stri_trim(x))
  all_values <- stringi::stri_unique(unlist(temp1))
  all_values

  finl <- detection(temp1, all_values)
  colnames(finl) <- all_values
  UpSetR::upset(finl, order.by = "freq", nsets = nsets)
}
