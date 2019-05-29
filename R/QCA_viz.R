#QCAviz

#The core functions  -----
#Libraries needed to be loaded
library(dplyr)
library(magrittr)

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
#' @export
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
#' @param df list of all the solutions or configurations
#' which are obtained from multiple QCA solutions
#' @param x QCA solutions in their raw form as produced
#' by \pkg{QCA} package
#' @return The function counts the individual solutions
#' or configurations  depending on the plot
#' function of this package. The output is a data frame.
#'
#' @export
detection <- function(df, x){
  mtr <- NULL
  for(l in x) {
    vctr <- as.numeric(stringi::stri_detect_fixed(df, l))
    mtr <- cbind(mtr, vctr)
    mtr <- as.data.frame(mtr)
  }

  return(mtr)
}

#'dt.selector()
#'
#' function that allows for selecting configurations
#' bsaed on consistency values before plitting
#'
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
  colnames(final_matrix) <- all_values
  UpSetR::upset(final_matrix, order.by = "freq", nsets = nsets)

}

#' conds_upset_h()
#'
#' Plot Function for the charting of conditions
#' intersection of the panel QCA output in a solution.
#'
#' @importFrom magrittr %>%
#' @import stringi
#' @param df is an object that contains hand-pooled (concatenated)
#' solutions produced by the \pkg{QCA} package.
#' This object includes only the solutions.
#' @param nsets an argument imported from the \pkg{UpSetR}
#' package. Determines the number of sets to graph.
#' @param type an argument to specify the panel dimension.
#' One can specify either within, between or
#' pooled types. Input is a string and there is
#' no default option for this argument.
#' @return The function returns an UpSetR-generated
#' visual represenation of the interconnecteds
#' between the conditions in a given dataset
#'
#' @export
conds_upset_h <- function (df, nsets) {
  temp1 <- unlist(df)
  temp1 <- purrr::map(temp1, function(x) stringi::stri_split_fixed(x, "*") %>% unlist())
  temp1 <- purrr::map(temp1, function(x) stringi::stri_split_fixed(x, "+") %>% unlist())
  all_values <- stringi::stri_unique(unlist(temp1))
  final_matrix <- plyr::ldply(temp1, function(y) comparison(x = all_values, y = y, num = T))
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
#' @export
config_upset <- function(df, const = FALSE, y, nsets) {

  if (!const) {
    df <- unlist(df$solution)
    temp1 <- purrr::map(df, function(x) stringi::stri_split_fixed(x,
                                                                  "+"))
    temp1 <- purrr::map(temp1, unlist)
    temp1 <- purrr::map(temp1, function(x) stringi::stri_split_fixed(x,
                                                                     " "))
  }
  else {
    temp1 <- dt.selector(df, con.thresh = y)
    temp1 <- purrr::map(temp1$config, unlist)
  }
  temp2 <- purrr::map(temp1, function(x) stringi::stri_trim(x))
  all_values <- stringi::stri_unique(unlist(temp1))
  all_values <- purrr::map(all_values, function(x) stringi::stri_trim(x))
  final_matrix <- plyr::ldply(temp1, function(y) comparison(x = all_values,
                                                            y = y, num = T))
  colnames(final_matrix) <- all_values
  UpSetR::upset(final_matrix, order.by = "freq", nsets = nsets)
}


#' config_upset_h()
#' a rewritten version
#' Plot Function for the charting of configuration
#' intersection of the panel QCA output in a solution.
#'
#' @importFrom magrittr %>%
#' @import stringi
#' @param df the data frame which has the
#' configurations extracted from the QCA solutions only
#' @param nsets an argument imported from the UpSetR
#' package. Determines the number of sets to graph.
#' @param type an argument to specify the panel dimension.
#' One can specify either within, between or
#' pooled types. Input is a string and there is
#' no default option for this argument.
#' @return The function returns an UpSetR-generated
#' visual represenation of the interconnecteds
#' between the conditions in a given dataset
#' @note the present version of config_upset
#' does not allow for selection of thresholds
#' as opposed to it's sibling function config_upset
#'
#' @export

config_upset_h1 <- function(df, nsets) {
  #preparing the data
  temp1 <- purrr::map(df, function(x) stringi::stri_trim(x))
  temp1 <- unlist(temp1)

  #preparing the configurations
  all_values <- purrr::map(df, function(x) stringi::stri_split_fixed(x, "+"))
  all_values <- stringi::stri_unique(unlist(all_values))

  #applying the function for detection
  finl <- detection(temp1, all_values)
  colnames(finl) <- all_values
  UpSetR::upset(finl, order.by = "freq", nsets = nsets)
}


#' config_upset_t()
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
config_upset_t <- function(df, nsets) {
  temp1 <- purrr::map(df, function(x) stringi::stri_split_fixed(x, "+") %>% unlist())
  temp1 <- purrr::map(temp1, function(x) stringi::stri_trim(x))
  all_values <- stringi::stri_unique(unlist(temp1))
  final_matrix <- plyr::ldply(temp1, function(y) comparison(x = all_values, y = y, num = T))
  colnames(final_matrix) <- all_values
  UpSetR::upset(final_matrix, order.by = "freq", nsets = nsets)
}


#' solutions_table()
#'
#' Plot Function for the table version of solutions
#' the special  function which allows to
#' chart the solutions in their entirety inside
#' a table which is transferrable to R markdown
#'
#' @importFrom magrittr %>%
#' @import pander
#' @param ls is a list of all solutions in their
#' entireity and row format
#' @return the function returns a table of solutions
#' and their frequency of occurrence in a desceinding
#' order (in the ready for rmarkwdon)
#'
#' @export
solutions_table <- function(ls) {

  #Working with the data
  temp3 <- purrr::map(ls, function(x) stringi::stri_split_fixed(x, " "))
  solutions1 <- Reduce(c, ls) %>% as.list()
  all_values3 <- unlist(unique(solutions1))

  #new function to paste the strings
  p <- function(..., sep=' + ') {
    paste(..., sep=sep, collapse=sep)
  }

  #applying the new function to my list
  j <- purrr::map(temp3, p)

  #binding the solutions
  b <- suppressWarnings(purrr::map_df(j,
                                      ~ data.frame(Content = .x),
                                      .id = "Solution"
  ))

  #putting into table and ordering
  tableX <- as.data.frame(table(b$Content))
  colnames(tableX) <- c("Solution", "Freq")
  newdf <- tableX %>% dplyr::arrange(desc(Freq))
  Solutions_table <- pander::pandoc.table(newdf)

  return(Solutions_table)
}


#'the solutions barchart
#'The documentation required
barplot <- function(ls){

  #Working with the data
  temp3 <- purrr::map(ls, function(x) stringi::stri_split_fixed(x, " "))
  solutions1 <- Reduce(c, ls) %>% as.list()
  all_values3 <- unlist(unique(solutions1))

  #new function to paste the strings
  p <- function(..., sep=' + ') {
    paste(..., sep=sep, collapse=sep)
  }

  #applying the new function to my list
  j <- purrr::map(temp3, p)

  #binding the solutions
  b <- suppressWarnings(purrr::map_df(j,
                                      ~ data.frame(Content = .x),
                                      .id = "Raw"
  ))

  #putting into table and ordering
  tableX <- as.data.frame(table(b$Content))
  colnames(tableX) <- c("Raw", "Freq")
  newdf <- tableX %>%
    dplyr::arrange(desc(Freq)) %>%
    dplyr::mutate(solution = paste0("sol_", 1:length(tableX$Raw))) %>%
    dplyr::mutate(solution = as.factor(solution)) %>%
    dplyr::mutate(solution = forcats::fct_reorder(solution, Freq, .desc = TRUE))

  #charting the plot
  plot2 <- ggplot2::ggplot(newdf, ggplot2::aes(x=solution, y = Freq, fill = Freq, order)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::theme(axis.text.x= ggplot2::element_text(angle=90,hjust=1,vjust=0.5))+
    ggplot2::geom_text(ggplot2::aes(label = Freq, y = Freq), size = 3, position = ggplot2::position_stack(vjust = 1.1)) +
    ggplot2::ggtitle("Distribution of Solutions") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))

  return(plot2)
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

