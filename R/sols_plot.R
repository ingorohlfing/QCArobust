#' sols_plot()
#'
#' the special plot function which allows to
#' chart the solutions in their entirety (with all
#' the configurations included)
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#' @param ls is a list of all solutions generated
#' through the iterative process
#' @return the function returns a chart which plots out
#' the number of times a specific solution occurs in a
#' given model. The solutions are checked against
#' multiple amount of iterations
#'
#' @export
sols_plot <- function(ls){

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
