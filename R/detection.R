#' Version of comparison function for configurations.
#'
#' @importFrom  stringi stri_detect_fixed
#'
#' @param df List of QCA solutions or configurations
#' derived from multiple truth table analyses performed
#' with \code{\link[QCA]{minimize}} from \code{\link{QCA}}
#' package
#'
#' @return A dataframe counting the individual solutions
#' or configurations.
detection <- function(df, x){
  mtr <- NULL
  for(l in x) {
    vctr <- as.numeric(stringi::stri_detect_fixed(df, l))
    mtr <- cbind(mtr, vctr)
    mtr <- as.data.frame(mtr)
  }

  return(mtr)
}
