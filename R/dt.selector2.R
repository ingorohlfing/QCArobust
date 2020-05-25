#' dt.selector2()
#'
#' function that allows for selecting specific
#' coverage threshold.
#'
#' @importFrom magrittr %>%
#' @import rlist
#' @import purrr
#' @param x output of the QCA in its entirety
#' produced by \pkg{QCA} package
#' @param con.thresh is the threshold set by a
#' researcher. Te default value equals to zero
#' @return The function returns a subset of
#' configurations that are larger than the selected
#' threshold value.
#'
#' @export
dt.selector2 <- function(x, cov.thresh){

  sol <- purrr::map(x$solution, function(x) stringi::stri_split_fixed(x, "+"))
  cov<- x$IC$incl[["inclS"]]


  output <- rlist::list.append(sol, cov)
  colnames(output) <- c("config", "cov")

  output <- as.data.frame(output)
  output <- output %>%
    filter(cov > cov.thresh) %>%
    select(cov)

  return(output)
}
