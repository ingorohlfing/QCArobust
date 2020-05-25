#' Preselecting configurations for analysis bsaed on minimum consistency value
#'
#' @importFrom magrittr %>%
#' @import stringi
#' @import rlist
#' @import purrr
#' @param x output of the QCA in its entirety
#' produced by \pkg{QCA} package
#' @param con.thresh Minimum consistency threshold for being included in
#' the analysis. Default is zero.
#'
#' @return A list of a subset of configurations meeting the threshold.
#'
#' @export
cons_minimum <- function(x, conthresh = 0) {

  sol <- purrr::map(x$solution, function(x) stringi::stri_split_fixed(x, "+"))
  cnst <- x$IC$incl.cov[["inclS"]]

  output <- rlist::list.append(sol, cnst)
  output <- rlist::list.cbind(output)
  colnames(output) <- c("config", "consist")

  output <- as.data.frame(output)
  output <- output %>% filter(consist > conthresh) %>% select(config)

  return(output)
}
