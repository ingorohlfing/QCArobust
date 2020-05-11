#QCAviz

#The core functions  -----


#' Main Functions
#' comparison()
#'
#' function compares matrices of solutions
#' to prepare the data for charting with the UpSetR
#'
#' @param x list of all the solutions or configurations
#' which are obtained from multiple QCA solutions
#' @param y QCA solutions in their raw form as produced
#' by \pkg{QCA} package
#' @param num is a default argument which checks
#' whether the inputs are numeric or not. The default
#' is set for false. This ensures that even if the input
#' is non-numeric, the function will read in the
#' data in a numeric format.
#' @return The function counts the individual solutions
#' or configurations  depending on the plot
#' function of this package. The output is a data frame.
#'
comparison <- function(x = all_values, y, num = F) {
  temp <- x %in% y

  if (num) {
    return(as.numeric(temp))
  } else {
    return(temp)
  }
}


#' detection()
#'
#' a new version of comparison function designed for the
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
#'
detection <- function(df, x){
  mtr <- NULL
  for(l in x) {
    vctr <- as.numeric(stringi::stri_detect_fixed(df, l))
    mtr <- cbind(mtr, vctr)
    mtr <- as.data.frame(mtr)
  }

  return(mtr)
}

#' dt.selector()
#'
#' function that allows for selecting configurations
#' bsaed on consistency values before plitting
#'
#' @importFrom magrittr %>%
#' @import stingi
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
dt.selector <- function(x, con.thresh = 0){

  sol <- purrr::map(x$solution, function(x) stringi::stri_split_fixed(x, "+"))
  cnst <- x$IC$incl.cov[["inclS"]]

  output <- rlist::list.append(sol, cnst)
  output <- rlist::list.cbind(output)
  colnames(output) <- c("config", "consist")

  output <- as.data.frame(output)
  output <- output %>%
    filter(consist > con.thresh) %>%
    select(config)

  return(output)
}


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


#' conds_upset()
#'
#' Function decomposes results into individual conditions
#' and creates intersection plots.
#'
#' @importFrom magrittr %>%
#' @import stringi
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


#' config_upset()
#'
#' Function decomposes results into individual configurations
#' and creates intersection plots.
#'
#' @importFrom magrittr %>%
#' @import stringi
#' @param df output extracted from the \pkg{QCA} package
#' in their entirety.
#' @param const is the argument of a function with binary
#' input. The default option - FALSE - indicates that all
#' of the QCA configurations will be plotted. If the argument
#' is set to TRUE, the function will only plot those
#' configurations which are above a chosen threshold: see
#' the following argument for threshold.
#' @param y is an argument with default value of zero. If
#' any other number is set for the given argument, only
#' those configurations which are above the set value for y
#' will be plotted by the function
#' @param nsets An argument imported from the \pkg{UpSetR}
#' package. Determines the number of sets to be plotted.
#' @return An \pkg{UpSetR}-generated intersection plot
#' presenting the frequency of individual configurations across
#' QCA solutions and their intersections across solutions.
#'
#' @export
config_upset <- function(df, const = FALSE, y, nsets) {

  if (!const) {
    temp1 <- unlist(df$solution)

  }
  else {
    temp1 <- dt.selector(df, con.thresh = y)
    temp1 <- purrr::map(temp1$config, unlist)
  }
  temp1 <- purrr::map(temp1, function(x) stringi::stri_trim(x))
  all_values <- stringi::stri_unique(unlist(temp1))
  all_values

  finl <- detection(temp1, all_values)
  colnames(finl) <- all_values
  UpSetR::upset(finl, order.by = "freq", nsets = nsets)
}



#' counting()
#'
#' Function that counts a specific string combination
#'
#' @importFrom magrittr %>%
#' @import stringi
#' @param list the data from within which we want to count
#' @param terms are the terms that we need to count
#' @return returns a the count of terms within the list of strings
#' @note this functiion uses the stri_count function of the
#' stringi function to count  a pattern in a list of strings
#'
#' @export
counting <- function(list, terms) {
  temp <- stringi::stri_count_fixed(list, terms)
  final <- count(temp)
  return(final)
}




#Datasets Documentation ----

#' Hinter-Leitner dataset
#' Data borrowed from Hinter and Leitner study
#' A dataset containing informaiton about 20 countres
#' used to understand the politic sof external
#' approval: Explaining the IMF's evaluation
#' of austerity programmes in those countries.
#'
#' @format A list with 20 observations of 10 variables
#' \describe{
#'   \item{case}{the case covered in the study. The country being analyzed}
#'   \item{pos}{the numeric value of pos}
#'   \item{reduction}{the numeric value of reduction}
#'   \item{duration}{the numeric value of duration}
#'   \item{ambition}{the numeric value of ambition}
#'   \item{DC}{the numeric value of DC}
#'   \item{EFF}{the numeric value of EFF}
#'   \item{CR}{the numeric value of CR}
#'   \item{COM}{the numeric value of COM}
#'   \item{PRG}{the numeric value of PRG}
#' }
#' @source \url{http://onlinelibrary.wiley.com/doi/10.1111/1475-6765.12142/suppinfo/}
"hinter"


#' Krogslund dataset
#' Data borrowed from Krogslund/Choi/Poertner study
#' A dataset containing informaiton about 5 variables
#' that are the result of a simulation
#'
#' @format A tibble with 40000 observations of 5 variables
#' \describe{
#'   \item{incl.cut1.val}{First inclusion cut}
#'   \item{incl.cut0.val}{Second inclusion cut}
#'   \item{n.cut.val}{the numeric value}
#'   \item{error}{the error}
#'   \item{Configurations}{Configurations vector}
#' }
#' @source \url{https://www.cambridge.org/core/journals/political-analysis/article/fuzzy-sets-on-shaky-ground-parameter-sensitivity-and-confirmation-bias-in-fsqca/B693F136C5158DF3C19686EC89522C23}
"hinter"
