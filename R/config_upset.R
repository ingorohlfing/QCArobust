#' Function decomposes results into individual configurations
#' and creates intersection plots.
#'
#' @importFrom magrittr %>%
#' @import stringi
#' @import purrr
#' @import UpSetR
#'
#' @param df output extracted from the \pkg{QCA} package
#' in their entirety.
#' @param const is the argument of a function with binary
#' input. The default option - FALSE - indicates that all
#' of the QCA configurations will be plotted. If the argument
#' is set to TRUE, the function will only plot those
#' configurations which are above a chosen threshold: see
#' the following argument for threshold.
#' @param y is an argument with default value of zero. If
#' any other number is set for the given argument, only
#' those configurations which are above the set value for y
#' will be plotted by the function
#' @param nsets An argument imported from the \pkg{UpSetR}
#' package. Determines the number of sets to be plotted.
#' @return An \pkg{UpSetR}-generated intersection plot
#' presenting the frequency of individual configurations across
#' QCA solutions and their intersections across solutions.
#'
#' @export
config_upset <- function(df, const = FALSE, y, nsets) {

  if (!const) {
    temp1 <- unlist(df$solution)

  }
  else {
    temp1 <- cons_minimum(df, consthresh = y)
    temp1 <- purrr::map(temp1$config, unlist)
  }
  temp1 <- purrr::map(temp1, function(x) stringi::stri_trim(x))
  all_values <- stringi::stri_unique(unlist(temp1))
  all_values

  finl <- detection(temp1, all_values)
  colnames(finl) <- all_values
  UpSetR::upset(finl, order.by = "freq", nsets = nsets)
}
