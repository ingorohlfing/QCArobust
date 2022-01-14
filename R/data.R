#' Datasets documentation

#' List of hypothetical solutions
#'
#' A list with ten hypothetical solutions derived from a truth table analysis.
#'
#' @format A list with 10 solutions
"solutions_example"

#' Krogslund dataset (model 1, ncut 1)
#' Data borrowed from Krogslund/Choi/Poertner study
#' A dataset containing informaiton about 5 variables
#' that are the result of a simulation analysis. This
#' dataset refers to a simulation analysis of original data
#' by Koenig-Archibugi.
#'
#' @format A tibble with 40000 observations and 5 variables
#' \describe{
#'   \item{incl.cut1.val}{First inclusion cut}
#'   \item{incl.cut0.val}{Second inclusion cut}
#'   \item{n.cut.val}{the numeric value}
#'   \item{error}{the error}
#'   \item{Configurations}{Configurations vector}
#' }
#' @source \url{https://doi.org/10.1093/pan/mpu016}
"KCP_model1_ncut1"


#' Skaaning dataset
#' Data is borrowed from the robustness analysis by Skaaning and Sven-Erik (2011)
#' The list of solutions presented in table 3 of the article. It  summarizes
#' results for the choice of different calibration anchors for crisp-set QCA.
#' @format A list of 244 solutions.
#' \describe{
#'   \item{solution1}{First solution}
#'   \item{solution2}{Second solution}
#'   \item{solution3}{Third solution}
#'   \item{solution4}{Fourth solution}
#'   \item{solution5}{Fifth solution}
#' }
#' @source \url{https://doi.org/10.1177/0049124111404818}
"Skaaning_table3"
