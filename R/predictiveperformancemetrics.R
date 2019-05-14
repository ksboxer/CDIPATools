
#' This script contains all the code for ML performance metrics for binary outcomes only.
#'
#' This function produces a confusion matrix - a table that displays the false positive (FP), false negative (FN), true positive(TP), and true negative (results) by comparing a set of predictions to true values.
#' The predictions can either be binary or continuous. For continuous predictions, a threshold for translating the results to binary classifications must be supplied. If the predictions are already binary, then pass in .5. (KP: Why not just null?)
#'
#' @param predictions vector of numerics, predicted values
#' @param outcomes vector of numerics, actual values/outcomes, if binary we need to a 0/1
#' @param threshold numeric, value between 0 and 1 to translate continuous predictions to binary classifications
#' @return list returns a list object that includes a confusion matrix table, accuracy, kappa statistics etc
#' @export
#' @examples
#' confusion_matrix(predictions = FakePredictionResults$est.risk.score,
#' outcomes = FakePredictionResults$true.risk.bin, threshold = 0.5)

confusion_matrix <- function(predictions, outcomes, threshold){

  predictions <- as.vector(predictions)

  outcomes <- as.vector(outcomes)
  outcomes <- factor(outcomes)

  predictions_threshold <- ifelse(predictions >= threshold, 1, 0)
  predictions_threshold <- factor(predictions_threshold)

  CM <- caret::confusionMatrix(predictions_threshold, outcomes, mode = "everything")
  return(CM)
}

#' Calculates the False Positive Rate (FPR)
#'
#' This function takes the predictions of a model,  (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the False Positive Rate.  Given that predictions need to be binary for the FPR to be calculated
#' you need to pass in a threshold to cut the predictions off.  If the predictions are already binary, then pass in .5
#' FPR = FP / (FP + TN)
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, false positive rate
#' @export

fpr <- function(predictions, outcomes, threshold){

  CM <- confusion_matrix(predictions, outcomes, threshold)

  table <- CM$table

  fp <- table[2,1]
  tn <- table[1,1]

  fpr <- fp / (fp + tn)
  return(fpr)
}

#' Calculates the False Negative Rate (FNR)
#'
#' This function takes the predictions of a model,  (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the False Negative Rate.  Given that predictions need to be binary for the FNR to be calculated
#' you need to pass in a threshold to cut the predictions off.  If the predictions are already binary, then pass in .5
#' FNR = FN / (FN + TP)
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, false negative rate
#' @export

fnr <- function(predictions, outcomes, threshold){

  CM <- confusion_matrix(predictions, outcomes, threshold)

  table <- CM$table

  fn <- table[1,2]
  tp <- table[2,2]

  fnr <- fn / (fn + tp)
  return(fnr)
}

#' Calculates the True Positive Rate (TPR)
#'
#' This function takes the predictions of a model,  (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the True Positive Rate.  Given that predictions need to be binary for the TPR to be calculated
#' you need to pass in a threshold to cut the predictions off.  If the predictions are already binary, then pass in .5
#' TPR = TP / (FN + TP)
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, true positive rate
#' @export

tpr <- function(predictions, outcomes, threshold){

  CM <- confusion_matrix(predictions, outcomes, threshold)

  table <- CM$table

  fn <- table[1,2]
  tp <- table[2,2]

  tpr <- tp / (fn + tp)
  return(tpr)
}

#' Calculates the True Negative Rate (TNR)
#'
#' This function takes the predictions of a model,  (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the True Negative Rate.  Given that predictions need to be binary for the TNR to be calculated
#' you need to pass in a threshold to cut the predictions off.  If the predictions are already binary, then pass in .5
#' TNR = TN / (TN + FP)
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, true negative rate
#' @export

tnr <- function(predictions, outcomes, threshold){

  CM <- confusion_matrix(predictions, outcomes, threshold)

  table <- CM$table

  fp <- table[2,1]
  tn <- table[1,1]

  tnr <- tn / (fp + tn)
  return(tnr)

}


#' Calculates area under the curve (AUC) for the receiver-operator cureve (ROC) for continious predictions, and actual predictions
#' KP: The summary above and below don't match.
#' This function takes the predictions of a model, (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the ROC curve, and then computes the AUC given the predicted values and actual values
#'
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @return numeric, returns AUC ROC value
#' @export

auc_roc <- function(predictions, outcomes){

  predictions <- as.vector(predictions)
  outcomes <- as.vector(outcomes)


  roc_obj <- pROC::roc(outcomes, predictions)
  auc_roc <- as.numeric(pROC::auc(roc_obj))
  return(auc_roc)
}



#' Calculates AUC Precision Recall  for continious predictions, and autual predictions
#'
#' This function takes the predictions of a model, (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the precision recall curve, and then gets the auc under the curve given the predicted values and actual values
#' Notes : baseline is ~.35, and perfect is 1
#'
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @return numeric, returns AUC precision recall value
#' @export

auc_pr <- function(predictions, outcomes){
  predictions <- as.vector(predictions)
  outcomes <- as.vector(outcomes)

  auc_pr <-  PRROC::pr.curve(predictions[outcomes==1], predictions[outcomes==0])
  return(auc_pr$auc.integral)
}

#' Calculate the Precision for predictions
#'
#' This function takes the predictions of a model, (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the precision based on the predictions.  Given that predictions need to binary for the precision calculates
#' you need to pass in a threshold to but the predictions off.  If the predictions are already binary, then pass in .5
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, returns precision value
#' @export
#'
precision <- function(predictions, outcomes, threshold){

  predictions <- as.vector(predictions)

  outcomes <- as.vector(outcomes)
  outcomes <- factor(outcomes)

  predictions_threshold <- ifelse(predictions >= threshold, 1, 0)
  predictions_threshold <- factor(predictions_threshold)

  precision <- caret::posPredValue(predictions_threshold, outcomes, positive = "1")
  return(precision)

}

#' Calculate the Recall for predictions
#'
#' This function takes the predictions of a model, (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the recall based on the predictions.  Given that predictions need to binary for the recall calculates
#' you need to pass in a threshold to but the predictions off.  If the predictions are already binary, then pass in .5
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, returns recall value
#' @export

recall <- function(predictions, outcomes, threshold){

  predictions <- as.vector(predictions)

  outcomes <- as.vector(outcomes)
  outcomes <- factor(outcomes)

  predictions_threshold <- ifelse(predictions >= threshold, 1, 0)
  predictions_threshold <- factor(predictions_threshold)

  recall <- caret::sensitivity(predictions_threshold, outcomes,  positive = "1")
  return(recall)

}

#' Calculate the Accuracy for predictions
#'
#' This function takes the predictions of a model, (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the accuracy based on the predictions.  Given that predictions need to binary for the accuracy calculates
#' you need to pass in a threshold to but the predictions off.  If the predictions are already binary, then pass in .5
#' Note: if there are imbalanced positives and negatives this metric might not be that useful
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, returns accuracy value
#' @export

accuracy <- function(predictions, outcomes, threshold){

  predictions <- as.vector(predictions)
  outcomes <- as.factor(as.vector(outcomes))
  predictions_threshold <- as.factor(ifelse(predictions >= threshold, 1, 0))

  CM <- caret::confusionMatrix(predictions_threshold, outcomes, mode = "prec_recall")
  accuracy <- as.vector(CM$overall["Accuracy"])
  return(accuracy)
}


#' Finds the threshold to cut continous predictions to maximize a given metric
#'
#' This function takes a given performance metric (precision, accuracy, or recall) that requires a threshold
#' and returns the threshold value that maximizes the specified metric
#'
#' @param func function, takes in other functions as arguments such as precision, accuracy and recall
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @return numeric, value from 0 to 1 that is the threshold that maximizes the metric func
#' @export

threshold_finder <- function(func, predictions,outcomes){

  i <- 0
  res <- data.frame()
  idx <- 1
  while(i <= 100){

    temp <- func(predictions,outcomes,i/100)
    res <- rbind(res, c(i/100, temp))


    i <- i +1
    idx <- idx +1
  }
  colnames(res)<- c("threshold", "res_")
  res <- res[order(-res$res_),]
  return(as.numeric(res$threshold[1]))
}
