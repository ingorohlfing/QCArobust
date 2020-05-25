#' Comparison of matrices preparing data for plotting (internal function)
#'
#' @param x List of QCA solutions or configurations
#' derived from multiple truth table analyses
#' @param y QCA solutions in their raw form as produced
#' by \code{\link[QCA]{minimize}} from \code{QCA} package
#' @param num Checks whether the input is  numeric or not. The default
#' is set to \code{FALSE}. This ensures that even if the input
#' is non-numeric, the function will read in the
#' data in a numeric format.
#'
#' @return A dataframe counting the individual solutions
#' or configurations.
comparison <- function(x = all_values, y, num = F) {
  temp <- x %in% y

  if (num) {
    return(as.numeric(temp))
  } else {
    return(temp)
  }
}
