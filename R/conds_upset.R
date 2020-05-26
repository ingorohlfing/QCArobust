#' conds_upset()
#'
#' Function decomposes results into individual conditions
#' and creates intersection plots.
#'
#' @importFrom magrittr %>%
#' @importFrom stringi stri_split_fixed stri_unique
#' @import purrr map
#' @importFrom plyr ldply
#' @import UpSetR
#' @param df Dataframe with solutions extracted
#' from the solutions of the \pkg{QCA} package
#' @param nsets An argument imported from the \pkg{UpSetR}
#' package. Determines the number of sets to be plotted.
#' @return An \pkg{UpSetR}-generated intersection plot
#' presenting the frequency of individual conditions across
#' QCA solutions and their intersections across solutions.
#'
#' @export
conds_upset <- function(df, nsets) {
  temp1 <- purrr::map(unlist(df), function(x) stringi::stri_split_fixed(x, "*") %>% unlist())
  temp1 <- purrr::map(temp1, function(x) stringi::stri_split_fixed(x, "+") %>% unlist())
  all_values <- stringi::stri_unique(unlist(temp1))
  final_matrix <- plyr::ldply(temp1, function(y) comparison(x = all_values, y = y, num = T))
  final_matrix$.id <- NULL
  colnames(final_matrix) <- all_values
  UpSetR::upset(final_matrix, order.by = "freq", nsets = nsets)

}
