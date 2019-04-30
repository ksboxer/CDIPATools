?#' Simulated Prediction and True Label dataset
#'
#' A simulated data frame containing 10,000 rows of predictions and true outcomes.
#'
#' @format A data frame with 10,000 rows and 5 variables:
#' \describe{
#'   \item{id}{row identification number}
#'   \item{est.risk.score}{predicted scores betweeen 0 and 1}
#'   \item{est.risk.bin}{predicted classificaitons of 0 or 1}
#'   \item{true.risk.score}{true scores between 0 and 1}
#'   \item{true.risk.bin}{true classifications of 0 or 1}
#' }
"FakePredictionResults"
