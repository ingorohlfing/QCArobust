#' Robustness for complete QCA models
#'
#' Aggregate over multiple model and calculate and plot
#' model frequencies.
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom dplyr mutate desc
#' @importFrom purrr map map_df
#' @importFrom stats na.omit
#'
#' @param ls List of QCA solutions
#' derived from multiple truth table analyses performed
#' with \code{\link[QCA]{minimize}} from \pkg{QCA}
#' package.
#' @param plot If set to \code{TRUE}, one produces a bar chart of frequency
#' of models. If set to \code{FALSE},
#' a dataframe with models and their frequencies is returned.
#' Default is \code{TRUE}.
#' @param plot_solutions Number of solutions to be plotted in
#' bar chart. Default is 5. If \code{plot} is set to \code{FALSE},
#' the dataframe shows all solutions regardless of what number one has
#' specified.
#'
#' @return If \code{plot} set to \code{TRUE}, a bar chart of
#' frequency of solutions is produced with \pkg{ggplot2} that can be
#' customized. If \code{plot} is set to \code{FALSE}, a dataframe with
#' solutions and frequencies is returned that can be further processed.
#'
#' @examples
#' data("solutions_example")
#' # plotting models with default setting
#' solutions_robust(solutions_example, plot = TRUE)
#' # plotting three solutions
#' solutions_robust(solutions_example, plot = TRUE, plot_solutions = 3)
#'
#' @export
solutions_robust <- function(ls, plot = TRUE, plot_solutions = 5) {
  if (class(ls) != "list") {
    stop('The object is not a list. Function only processes lists.')
  }
  if (length(ls) == 1) {
    stop('One needs at least two QCA models to use the function.')
  }
  else {
    # preprocessing the list
    temp3 <- purrr::map(ls, function(x)
      suppressWarnings(stringi::stri_split_fixed(x, " ")))

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
}
