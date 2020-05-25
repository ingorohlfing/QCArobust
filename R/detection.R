#' New version of comparison function designed for the
#' configurations. Needs further reworking to get rid
#' of the for loop at later stages
#'
#' @import stringi
#' @param df list of all the solutions or configurations
#' which are obtained from multiple QCA solutions
#' @param x QCA solutions in their raw form as produced
#' by \pkg{QCA} package
#' @return The function counts the individual solutions
#' or configurations  depending on the plot
#' function of this package. The output is a data frame.
detection <- function(df, x){
  mtr <- NULL
  for(l in x) {
    vctr <- as.numeric(stringi::stri_detect_fixed(df, l))
    mtr <- cbind(mtr, vctr)
    mtr <- as.data.frame(mtr)
  }

  return(mtr)
}
