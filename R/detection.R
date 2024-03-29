#' Version of comparison function for configurations.
#'
#' @importFrom  stringi stri_detect_fixed
#'
#' @param ls List of QCA solutions or configurations
#' derived from multiple truth table analyses performed
#' with \code{\link[QCA]{minimize}} from \pkg{QCA}
#' package
#'
#' @param x a vector of all unique values derived from the
#' list of QCA solutions
#'
#' @return A list counting the individual solutions
#' or configurations.
detection <- function(ls, x){
  mtr <- NULL
  for(l in x) {
    vctr <- as.numeric(stringi::stri_detect_fixed(ls, l))
    mtr <- cbind(mtr, vctr)
    mtr <- as.data.frame(mtr)
  }

  return(mtr)
}
