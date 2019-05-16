library(CDIPATools)
library(testthat)
context("Testing Predictive Performance Metrics Functions")


test_that("Confusion Metric works properly -- specifically testing thresholding",{
  expect_equal(confusion_matrix(c(1,0,1), c(0, 0, 1), .5), caret::confusionMatrix(as.factor(c(1,0,1)), as.factor(c(0, 0, 1)), mode = "everything"))
  expect_equal(confusion_matrix(c(.6,.01,.51), c(0, 0, 1), .5), caret::confusionMatrix(as.factor(c(1,0,1)), as.factor(c(0, 0, 1)), mode = "everything"))
  expect_equal(confusion_matrix(c(.6,.499,.5), c(0, 0, 1), .5), caret::confusionMatrix(as.factor(c(1,0,1)), as.factor(c(0, 0, 1)), mode = "everything"))
  expect_equal(confusion_matrix(c(.6,.0001,.001), c(0, 0, 1), .001), caret::confusionMatrix(as.factor(c(1,0,1)), as.factor(c(0, 0, 1)), mode = "everything"))
})


test_that("FPR function",{
  expect_equal(fpr(c(1,0,1), c(0, 0, 1), .5), .5)
  expect_equal(fpr(c(1,0,1), c(0, 0, 1), .5), (caret::confusionMatrix(as.factor(c(1,0,1)), as.factor(c(0, 0, 1)), mode = "everything")$table[2,1]) / (caret::confusionMatrix(as.factor(c(1,0,1)), as.factor(c(0, 0, 1)), mode = "everything")$table[2,1] + caret::confusionMatrix(as.factor(c(1,0,1)), as.factor(c(0, 0, 1)), mode = "everything")$table[1,1]))
  expect_equal(fpr(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), (caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[2,1]) / (caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[2,1] + caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[1,1]))
})

test_that("FNR function",{
  expect_equal(fnr(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), .5)
  expect_equal(fnr(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), (caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[1,2]) / (caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[1,2] + caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[2,2]))
})

test_that("TPR function",{
  expect_equal(tpr(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), .5)
  expect_equal(tpr(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), (caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[2,2]) / (caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[2,2] + caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[1,2]))
})

test_that("TNR function",{
  expect_equal(tnr(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), .5)
  expect_equal(tnr(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), (caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[1,1]) / (caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[1,1] + caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[2,1]))
})

test_that("AUC ROC function",{
  expect_equal(auc_roc(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1)), as.numeric(pROC::auc(pROC::roc( c(0, 0, 1,1,1,1), c(1,0,1,0,0,1)))))
  expect_equal(auc_roc(c(.56,.002,.1,.67,.3,.88), c(0, 0, 1,1,1,1)), as.numeric(pROC::auc(pROC::roc( c(0, 0, 1,1,1,1), c(.56,.002,.1,.67,.3,.88)))))
})

test_that("AUC PR function",{
  expect_equal(auc_pr(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1)), PRROC::pr.curve(c(1,0,0,1), c(1,0))$auc.integral)
  expect_equal(auc_pr(c(.56,.002,.1,.67,.3,.88), c(0, 0, 1,1,1,1)), PRROC::pr.curve(c(.1,.67,.3,.88), c(.56,.002))$auc.integral)
})

test_that("Precision function",{
  expect_equal(precision(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), (2/3))
  expect_equal(precision(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), (caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[2,2]) / (caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[2,2] + caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[2,1]))
  expect_equal(precision(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), as.numeric(caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)),  as.factor(c(0, 0, 1,1,1,1)), mode = "everything", positive = "1")$byClass["Precision"]))
})

test_that("Recall function",{
  expect_equal(recall(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), .5)
  expect_equal(recall(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), (caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[2,2]) / (caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[2,2] + caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[1,2]))
  expect_equal(recall(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), as.numeric(caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)),  as.factor(c(0, 0, 1,1,1,1)), mode = "everything", positive = "1")$byClass["Recall"]))
})

test_that("Accuracy function",{
  expect_equal(accuracy(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), .5)
  expect_equal(accuracy(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), as.vector(caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "prec_recall")$overall["Accuracy"]))
  #expect_equal(recall(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), as.numeric(caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)),  as.factor(c(0, 0, 1,1,1,1)), mode = "everything", positive = "1")$byClass["Recall"]))
})