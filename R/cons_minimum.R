#' Filtering configurations from results using minimum consistency value
#'
#' @importFrom magrittr %>%
#' @importFrom stringi stri_split_fixed
#' @import rlist
#' @importFrom purrr map
#' @importFrom dplyr select filter
#'
#' @param x List of solutions generated with \code{\link{QCA}} package
#' @param consthresh Minimum consistency threshold for for filtering
#' configurations. Default is zero.
#'
#' @return A list of configurations meeting the consistency threshold.
#'
#' @export
cons_minimum <- function(x, consthresh = 0) {

  sol <- purrr::map(x$solution, function(x) stringi::stri_split_fixed(x, "+"))
  cnst <- x$IC$incl.cov[["inclS"]]

  output <- rlist::list.append(sol, cnst)
  output <- rlist::list.cbind(output)
  colnames(output) <- c("config", "consistency")

  output <- as.data.frame(output)
  output <- output %>% filter(consistency > consthresh) %>% select(config)

  return(output)
}
