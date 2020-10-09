#' sols_plot()
#'
#' Plots the frequency of solutions using a bar chart.
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#'
#' @param ls List of QCA solutions or configurations
#' derived from multiple truth table analyses performed
#' with \code{\link[QCA]{minimize}} from \pkg{QCA}
#' package.
#'
#' @return A bar chart created with \code{\link{ggplot2}} summarizing
#' the frequency of solutions in the data.
#'
#' @export
sols_plot <- function(ls) {

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

  # creating rotated bar chart
  plot2 <- ggplot2::ggplot(data = newdf,
                           ggplot2::aes(x = solution, y = Freq)) +
    ggplot2::geom_bar(stat = "identity") +
    coord_flip() + theme_minimal() +
    scale_x_discrete("Solution ID") + scale_y_continuous("Frequency") +
    ggplot2::ggtitle("Frequency of solutions")

  return(plot2)
}
