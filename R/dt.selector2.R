#' Filtering configurations based on coverage threshold.
#'
#' @importFrom magrittr %>%
#' @importFrom rlist list.append
#' @importFrom purrr map
#'
#' @param x List of QCA solutions or configurations
#' derived from multiple truth table analyses performed
#' with \code{\link[QCA]{minimize}} from \code{\link{QCA}}
#' package
#' @param convthresh Coverage threshold for filtering
#' solutions or configurations. Default value is 0.
#'
#' @return A dataframe with configurations meeting the coverage
#' threshold.
#'
#' @export
dt.selector2 <- function(x, covthresh){

  sol <- purrr::map(x$solution, function(x) stringi::stri_split_fixed(x, "+"))
  cov<- x$IC$incl[["inclS"]]


  output <- rlist::list.append(sol, cov)
  colnames(output) <- c("config", "cov")

  output <- as.data.frame(output)
  output <- output %>%
    filter(cov > covthresh) %>%
    select(cov)

  return(output)
}
