context("Testing Predictive Performance Metrics Functions")
library(CDIPATools)

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
  expect_equal(fnr(c(1,0,1,0,0,1), c(0, 0, 1,1,1,1), .5), (caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[2,2]) / (caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[2,2] + caret::confusionMatrix(as.factor(c(1,0,1,0,0,1)), as.factor(c(0, 0, 1,1,1,1)), mode = "everything")$table[1,2]))
})