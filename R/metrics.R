
# This script contains all the code for ML performance metrics
KP: Lets rename this script to be more specific about what it contains (metrics could be anything). Maybe predictive performance metrics?

#' Confusion Matrix
#'
#' This function produces a confusion matrix - a table that displays the false positive (FP), false negative (FN), true positive(TP), and true negative (results) by comparing a set of predictions to true values.
#' The predictions can either be binary or continuous. For continuous predictions, a threshold for translating the results to binary classifications must be supplied. If the predictions are already binary, then pass in .5. (KP: Why not just null?)
#'
#' @param predictions list (KP: isn't it a numeric vector?) of numerics,  predicted values
#' @param labels (KP: let's limit jargon and say call this "outcomes" or "trueOutcomes" list of numerics (KP: again use correct R object), actual values/outcomes (KP: if binary, need to be 0/1 yes?)
#' @param threshold numeric, value between 0 and 1 to translate continuous predictions to binary classifications
#' @return table (KP: is this an object name or object classification?), table of integers with FP, FN, TP, TN.
#' @export # KP I don't think the return is correct. It returns a lot more according to the vignette.


get_confusion_matrix <- function(predictions, labels, threshold){ # KP: can you just have threshold=NULL as default?
  #KP Maybe not use "get_" Not sure why that is helpful. Maybe we should think about naming conventions? I often use "get" for functions that are designed to grab some subset of information from an R object. Not that we have to use that convention, but as an example of how we might use names.


  predictions <- as.vector(predictions)

  labels <- as.vector(labels)
  labels <- factor(labels)

  predictions_threshold <- ifelse(predictions >= threshold, 1, 0)
  predictions_threshold <- factor(predictions_threshold)

  CM <- caret::confusionMatrix(predictions_threshold, labels, mode = "everything")
  return(CM)
}

KP: What is the point of these functions when these values have already been computed in the above get_confusion_matrix function?

#' Calculates the False Positive Rate (FPR)
#'
#' This function takes the predictions of a model,  (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the False Positive Rate.  Given that predictions need to be binary for the FPR to be calculated
#' you need to pass in a threshold to cut the predictions off.  If the predictions are already binary, then pass in .5
#' FPR = FP / (FP + TN)
#'
#' @param predictions list of numerics,  predicted values
#' @param labels list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, false positive rate
#' @export

get_fpr <- function(predictions, labels, threshold){

  CM <- get_confusion_matrix(predictions, labels, threshold)

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
#' @param labels list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, false negative rate
#' @export

get_fnr <- function(predictions, labels, threshold){

  CM <- get_confusion_matrix(predictions, labels, threshold)

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
#' @param labels list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, true positive rate
#' @export

get_tpr <- function(predictions, labels, threshold){

  CM <- get_confusion_matrix(predictions, labels, threshold)

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
#' @param labels list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, true negative rate
#' @export

get_tnr <- function(predictions, labels, threshold){

  CM <- get_confusion_matrix(predictions, labels, threshold)

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
#' @param labels list of numerics, actual values/outcomes
#' @return numeric, returns AUC ROC value
#' @export

get_auc_roc <- function(predictions, labels){

  predictions <- as.vector(predictions)
  labels <- as.vector(labels)


  roc_obj <- pROC::roc(labels, predictions)
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
#' @param labels list of numerics, actual values/outcomes
#' @return numeric, returns AUC precision recall value
#' @export

get_auc_pr <- function(predictions, labels){
  predictions <- as.vector(predictions)
  labels <- as.vector(labels)

  auc_pr <-  PRROC::pr.curve(predictions[labels==1], predictions[labels==0])
  return(auc_pr$auc.integral)
}

#' Calculate the Precision for predictions
#'
#' This function takes the predictions of a model, (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the precision based on the predictions.  Given that predictions need to binary for the precision calculates
#' you need to pass in a threshold to but the predictions off.  If the predictions are already binary, then pass in .5
#'
#' @param predictions list of numerics,  predicted values
#' @param labels list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, returns precision value
#' @export
#'
get_precision <- function(predictions, labels, threshold){

  predictions <- as.vector(predictions)

  labels <- as.vector(labels)
  labels <- factor(labels)

  predictions_threshold <- ifelse(predictions >= threshold, 1, 0)
  predictions_threshold <- factor(predictions_threshold)

  precision <- caret::posPredValue(predictions_threshold, labels, positive = "1")
  return(precision)

}

#' Calculate the Recall for predictions
#'
#' This function takes the predictions of a model, (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the recall based on the predictions.  Given that predictions need to binary for the recall calculates
#' you need to pass in a threshold to but the predictions off.  If the predictions are already binary, then pass in .5
#'
#' @param predictions list of numerics,  predicted values
#' @param labels list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, returns recall value
#' @export

get_recall <- function(predictions, labels, threshold){

  predictions <- as.vector(predictions)

  labels <- as.vector(labels)
  labels <- factor(labels)

  predictions_threshold <- ifelse(predictions >= threshold, 1, 0)
  predictions_threshold <- factor(predictions_threshold)

  recall <- caret::sensitivity(predictions_threshold, labels,  positive = "1")
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
#' @param labels list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, returns accuracy value
#' @export

get_accuracy <- function(predictions, labels, threshold){

  predictions <- as.vector(predictions)
  labels <- as.factor(as.vector(labels))
  predictions_threshold <- as.factor(ifelse(predictions >= threshold, 1, 0))

  CM <- caret::confusionMatrix(predictions_threshold, labels, mode = "prec_recall")
  accuracy <- as.vector(CM$overall["Accuracy"])
  return(accuracy)
}


#' Finds the threshold to cut continous predictions to maximize a given metric
#'
#' This function takes a given performance metric (precision, accuracy, or recall) that requires a threshold
#' and returns the threshold value that maximizes the specified metric
#'
#' @param func function, function that takes arguments, predictions, labels, and threshold
# #KP Tell the user the options here. But if we discard those functions, we'll need to revise this function.

#' @param predictions list of numerics,  predicted values
#' @param labels list of numerics, actual values/outcomes
#' @return numeric, value from 0 to 1 that is the threshold that maximizes the metric func
#' @export

threshold_finder <- function(func, predictions,labels){

  i <- 0
  res <- data.frame()
  idx <- 1
  while(i <= 100){

    temp <- func(predictions,labels,i/100)
    res <- rbind(res, c(i/100, temp))


    i <- i +1
    idx <- idx +1
  }
  colnames(res)<- c("threshold", "res_")
  res <- res[order(-res$res_),]
  return(as.numeric(res$threshold[1]))
}
