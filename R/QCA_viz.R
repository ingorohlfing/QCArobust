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


#' conds_upset()
#'
#' Function decomposes results into individual conditions
#' and creates intersection plots.
#'
#' @importFrom magrittr %>%
#' @import stringi
#' @param df Dataframe with configurations extracted
#' from the QCA solutions
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

#' config_upset()
#'
#' Function decomposes results into individual configurations
#' and creates intersection plots.
#'
#' @importFrom magrittr %>%
#' @import stringi
#' @param df Dataframe with configurations extracted
#' from the QCA solutions
#' @param nsets An argument imported from the \pkg{UpSetR}
#' package. Determines the number of sets to be plotted.
#' @return An \pkg{UpSetR}-generated intersection plot
#' presenting the frequency of individual configurations across
#' QCA solutions and their intersections across solutions.
#' @export
config_upset <- function(df, nsets) {
  df <- unlist(df)
  temp1 <- purrr::map(df, function(x) stringi::stri_split_fixed(x, "+"))
  temp1 <- purrr::map(temp1, unlist)
  temp1 <- purrr::map(temp1, function(x) stringi::stri_split_fixed(x, " "))
  temp1 <- purrr::map(temp1, function(x) stringi::stri_trim (x))
  all_values <- stringi::stri_unique(unlist(temp1))
  all_values <- purrr::map(all_values, function(x) stringi::stri_trim(x))
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

#HAND - POOLED DATA VISUALS

#' conds_upset_h()
#'
#' Plot Function for the charting of conditions
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
#' @note the chart generated by this function is
#' different from that generated by the sols_upset
#'
#' @export
conds_upset_h <- function (df, nsets) {
  temp1 <- purrr::map(df, function(x) stringi::stri_split_fixed(x, "*") %>% unlist())
  temp1 <- purrr::map(temp1, function(x) stringi::stri_split_fixed(x, "+") %>% unlist())
  all_values <- stringi::stri_unique(unlist(temp1))
  final_matrix <- plyr::ldply(temp1, function(y) comparison(x = all_values, y = y, num = T))
  colnames(final_matrix) <- all_values
  UpSetR::upset(final_matrix, order.by = "freq", nsets = nsets)

}

#' config_upset_h()
#'
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
#' @note the chart generated by this function is
#' different from that generated by the sols_upset
#'
#' @export
config_upset_h <- function(df, nsets) {
  temp1 <- purrr::map(df, function(x) stringi::stri_split_fixed(x, "+"))
  all_values <- stringi::stri_unique(unlist(temp1))
  final_matrix <- plyr::ldply(temp1, function(y) comparison(x = all_values, y = y, num = T))
  colnames(final_matrix) <- all_values
  UpSetR::upset(final_matrix, order.by = "freq", nsets = nsets)
}



#Rewriting the config_functions with the consistency ----
#selector

#a single solution
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


#list selection ----
list.selector <- function(x, con.thresh = 0){

  solutions <- purrr::map((x), function(x) x[["solution"]])
  solutions <- rlist::list.stack(solutions)
  solutions <- as.data.frame(solutions)

  cnst <- purrr::map(x, function(x) x$IC$incl.cov[["inclS"]])
  cnst <- as.data.frame(rlist::list.cbind(cnst))
  cnst <- reshape::melt(cnst)

  output <- cbind(solutions$V1, cnst$value)
  colnames(output) <- c("config", "consist")

  output <- data.frame(output)
  output$consist <- as.numeric(as.character(output$consist))
  output <- output %>%
    filter(consist > con.thresh) %>%
    select(config)

  return(output)
}

#the config_functions
config_upset_1 <- functions(x, y, nsets){
  temp1 <- dt.selector(x, con.thresh = y)
  temp1 <- purrr::map(temp1$config, unlist)
  temp1 <- purrr::map(temp1, function(x) stringi::stri_trim(x))
  all_values <- stringi::stri_unique(unlist(temp1))
  all_values <- purrr::map(all_values, function(x) stringi::stri_trim(x))
  final_matrix <- plyr::ldply(temp1, function(y) comparison(x = all_values, y = y, num = T))
  colnames(final_matrix) <- all_values
  UpSetR::upset(final_matrix, order.by = "freq", nsets = nsets)
}

#the hand-pooled config function
config_upset_h1 <- function(x, y, nsents){
  temp1 <- list.selector(x, con.thresh = y)
  temp1 <- purrr::map(temp1$config, unlist)
  temp1 <- purrr::map(temp1, function(x) stringi::stri_trim(x))
  all_values <- stringi::stri_unique(unlist(temp1))
  final_matrix <- plyr::ldply(temp1, function(y) comparison (x = all_values, y = y, num = T))
  colnames(final_matrix) <- all_values
  UpSetR::upset(final_matrix, order.by = "freq", nsets = nsets)
}






