#' Simulated Prediction and True Label dataset
#'
#' A dataset containing 10,000 rows of randomly generated classification predictions,
#' with corresponding probability scores, and binary true labels, with corresponding
#' true risk scores
#'
#' @format A data frame with 10,000 rows and 5 variables:
#' \describe{
#'   \item{id}{sequential id, numeric}
#'   \item{est.risk.score}{0-1, predicted probability}
#'   \item{est.risk.bin}{0 or 1, predicted binary classification label}
#'   \item{true.risk.score}{0-1, true probability risk score}
#'   \item{true.risk.bin}{0 or 1, true binary label}
#' }

"fake_predictions_data"