#' Datasets documentation


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
#' @source \url{https://doi.org/10.1093/pan/mpu016}
"Krogslund"

#' Skaaning dataset
#' Data is borrowed from the robustness analysis by Skaaning and Sven-Erik (2011)
#' The list of solutions presented in table 3 of the article. It  summarizes
#' results for the choice of different calibration anchors for crisp-set QCA.
#' @format A list of solutions.
#' \describe{
#'   \item{solution1}{First solution}
#'   \item{solution2}{Second solution}
#'   \item{solution3}{Third solution}
#'   \item{solution4}{Fourth solution}
#'   \item{solution5}{Fifth solution}
#' }
#' @source \url{http://10.1177/0049124111404818}
"Skaaning_csanchors"


