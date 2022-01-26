#' Evaluate robustness of complete QCA models
#'
#' Aggregate over multiple solutions and calculate and plot
#' model frequencies.
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom dplyr mutate
#' @importFrom purrr map map_df
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
#' the dataframe shows all solutions regardless of what number has been
#' specified.
#'
#' @return If \code{plot} set to \code{TRUE}, a bar chart of
#' frequency of solutions is produced with \code{\link{ggplot2}}.
#' If \code{plot} is set to \code{FALSE}, a dataframe with
#' solutions and frequencies.
#'
#' @examples
#' data("Skaaning_table3")
#' # plotting models with default setting
#' solutions_robust(Skaaning_table3, plot = TRUE)
#' # plotting three solutions
#' solutions_robust(Skaaning_table3, plot = TRUE, plot_solutions = 3)
#'
#' @export
solutions_robust <- function(ls, plot = TRUE, plot_solutions = 5) {

  # preprocessing the list
  temp3 <- purrr::map(ls, function(x) suppressWarnings(stringi::stri_split_fixed(x, " ")))

  #adding the space between special characters
  temp3 <- lapply(unlist(temp3), FUN = function(t) gsub(pattern = "[*]",
                                                        replacement = " * ",
                                                        x = t))


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
                                      .id = "Model"
  ))

  # processing dataframe for outputting and plotting
  tableX <- as.data.frame(table(b$Content))
  colnames(tableX) <- c("Model", "Frequency")
  newdf <- tableX %>%
    dplyr::arrange(desc(Frequency)) %>%
    dplyr::mutate(solutionid = 1:length(tableX$Model)) %>%
    dplyr::mutate(solutionid = as.factor(solutionid)) %>%
    dplyr::mutate(solutionid = forcats::fct_reorder(solutionid, Frequency,
                                                    .desc = TRUE)) %>%
    dplyr::rename(`Model ID` = solutionid)
  # returning dataframe
  if(plot == FALSE) {
    return(newdf)
  }
  else {
    # subsetting dataframe to specified number of solutions
    newdf <- newdf[1:plot_solutions, ]
    # removing rows if plot_solutions > number of solutions in data
    newdf <- na.omit(newdf)
    # creating rotated bar chart
    plot2 <- ggplot2::ggplot(data = newdf,
                             ggplot2::aes(x = `Model ID`, y = Frequency)) +
      ggplot2::geom_bar(stat = "identity") +
      coord_flip() + theme_minimal() +
      scale_x_discrete("Model ID") + scale_y_continuous("Frequency") +
      ggplot2::ggtitle("Frequency of models")

    return(plot2)
  }
}
