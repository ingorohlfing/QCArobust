#' sols_aggregate()
#'
#' Aggregates over solutions by calculating and plotting
#' their frequencies.
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#'
#' @param ls List of QCA solutions or configurations
#' derived from multiple truth table analyses performed
#' with \code{\link[QCA]{minimize}} from \pkg{QCA}
#' package.
#' @param plot If set to \code{TRUE}, a bar chart of frequency
#' of solutions is produced. If set to \code{FALSE},
#' a dataframe with solutions and frequencies is returned.
#' Default is \code{TRUE}.
#' @param plot_solutions Number of solutions to be plotted in
#' bar chart. Default is 5. If \code{plot} is set to \code{FALSE},
#' the dataframe shows all solutions regardless of the number
#' specified for plotting.
#'
#' @return If \code{plot} set to \code{TRUE}, a bar chart of
#' frequency of solutions is produced with \code{\link{ggplot2}}.
#' If \code{plot} is set to \code{FALSE}, a dataframe with
#' solutions and frequencies.
#'
#' @export
sols_aggregate <- function(ls, plot = T, plot_solutions = 5) {

  # preprocessing the list
  temp3 <- purrr::map(ls, function(x) stringi::stri_split_fixed(x, " "))
  solutions1 <- Reduce(c, ls) %>% as.list()
  all_values3 <- unlist(unique(solutions1))

  # new function to paste the strings
  p <- function(..., sep = ' + ') {
    paste(..., sep = sep, collapse = sep)
  }

  # applying the new function to my list
  j <- purrr::map(temp3, p)

  # binding the solutions into a dataframe
  b <- suppressWarnings(purrr::map_df(j,
                                      ~ data.frame(Content = .x),
                                      .id = "Raw"
  ))

  # processing dataframe for outputting and plotting
  tableX <- as.data.frame(table(b$Content))
  colnames(tableX) <- c("Raw", "Freq")
  newdf <- tableX %>%
    dplyr::arrange(desc(Freq)) %>%
    dplyr::mutate(solution = 1:length(tableX$Raw)) %>%
    dplyr::mutate(solution = as.factor(solution)) %>%
    dplyr::mutate(solution = forcats::fct_reorder(solution, Freq, .desc = TRUE))
  # returning dataframe
  if(plot == F) {
    return(newdf)
  }
  else {
    # subsetting dataframe to specified number of solutions
    newdf <- newdf[1:plot_solutions, ]
    # removing rows if plot_solutions > number of solutions in data
    newdf <- na.omit(newdf)
    # creating rotated bar chart
    plot2 <- ggplot2::ggplot(data = newdf,
                             ggplot2::aes(x = solution, y = Freq)) +
      ggplot2::geom_bar(stat = "identity") +
      coord_flip() + theme_minimal() +
      scale_x_discrete("Solution ID") + scale_y_continuous("Frequency") +
      ggplot2::ggtitle("Frequency of solutions")

    return(plot2)
  }
}
