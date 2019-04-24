
#' @description This script contains all the code for ML performance metrics


#' Calculates Confusion Matrix (TP, FP, FN, TN)
#'
#' This function takes the predictions of a model,  (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the confusion metrics based on the predictions.  Given that predictions need to be binary for the confusion matrix to work
#' you need to pass in a threshold to cut the predictions off.  If the predictions are already binary, then pass in .5
#'
#' @param predictions list of numerics,  predicted values
#' @param labels list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return table, table of integers with FP, FN, TP,R
get_confusion_matrix <- function(predictions, labels, threshold){

  predictions <- as.vector(predictions)

  labels <- as.vector(labels)
  labels <- factor(labels)

  predictions_threshold <- ifelse(predictions >= threshold, 1, 0)
  predictions_threshold <- factor(predictions_threshold)

  CM <- confusionMatrix(predictions_threshold, labels, mode = "everything")
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
#' @param labels list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, false positive rate

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

get_tnr <- function(predictions, labels, threshold){

  CM <- get_confusion_matrix(predictions, labels, threshold)

  table <- CM$table

  fp <- table[2,1]
  tn <- table[1,1]

  tnr <- tn / (fp + tn)
  return(tnr)

}


#' Calculates AUC ROC for continious predictions, and autual predictions
#'
#' This function takes the predictions of a model, (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the roc curve, and then gets the auc under the curve given the predicted values and actual values
#' Notes : baseline is .5, and perfect is 1
#'
#' @param predictions list of numerics,  predicted values
#' @param labels list of numerics, actual values/outcomes
#' @return numeric, returns AUC ROC value

get_auc_roc <- function(predictions, labels){

  predictions <- as.vector(predictions)
  labels <- as.vector(labels)


  roc_obj <- roc(labels, predictions)
  auc_roc <- as.numeric(auc(roc_obj))
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

get_auc_pr <- function(predictions, labels){
  predictions <- as.vector(predictions)
  labels <- as.vector(labels)

  auc_pr <-  pr.curve(predictions[labels==1], predictions[labels==0])
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
get_precision <- function(predictions, labels, threshold){

  predictions <- as.vector(predictions)

  labels <- as.vector(labels)
  labels <- factor(labels)

  predictions_threshold <- ifelse(predictions >= threshold, 1, 0)
  predictions_threshold <- factor(predictions_threshold)

  precision <- posPredValue(predictions_threshold, labels, positive = "1")
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

get_recall <- function(predictions, labels, threshold){

  predictions <- as.vector(predictions)

  labels <- as.vector(labels)
  labels <- factor(labels)

  predictions_threshold <- ifelse(predictions >= threshold, 1, 0)
  predictions_threshold <- factor(predictions_threshold)

  recall <- sensitivity(predictions_threshold, labels,  positive = "1")
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

get_accuracy <- function(predictions, labels, threshold){

  predictions <- as.vector(predictions)
  labels <- as.factor(as.vector(labels))
  predictions_threshold <- as.factor(ifelse(predictions >= threshold, 1, 0))

  CM <- confusionMatrix(predictions_threshold, labels, mode = "prec_recall")
  accuracy <- as.vector(CM$overall["Accuracy"])
  return(accuracy)
}


#' Finds the threshold to cut continous predictions to maximize a given metric
#'
#' This function takes a given performance metric (precision, accuracy, or recall) that requires a threshold
#' and returns the threshold value that maximizes the specified metric
#'
#' @param func function, function that takes arguments, predictions, labels, and threshold
#' @param predictions list of numerics,  predicted values
#' @param labels list of numerics, actual values/outcomes
#' @return numeric, value from 0-1 that is the threshold that maximizes the metric func

theshold_finder <- function(func, predictions,labels){

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
