#' Function decomposes results into individual conditions
#' and creates intersection plots.
#'
#' @importFrom magrittr %>%
#' @importFrom stringi stri_split_fixed stri_unique
#' @importFrom purrr map
#' @importFrom plyr ldply
#' @import UpSetR
#'
#' @param df Dataframe or list of QCA solutions or configurations
#' derived from multiple truth table analyses performed
#' with \code{\link[QCA]{minimize}} from \code{\link{QCA}}
#' package
#' @param nsets Specifies number of sets to be plotted. Argument
#' imported from the \code{\link{upset}} function from \pkg{UpSetR}.
#'
#' @return A plot presenting the frequency of individual
#' conditions and their co-occurrences across QCA solutions.
#'
#' @examples
#' sols <- list(c(replicate("A", 2), replicate("AB+c", 2), replicate("A+c", 2)))
#' conds_upset(sols)
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
