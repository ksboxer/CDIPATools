context("Testing Predictive Performance Metrics Functions")
library(CDIPATools)

test_that("Confusion Metric works properly",{
  expect_equal(confusion_matrix(c(1,0,1), c(0, 0, 1), .5), caret::confusionMatrix(as.factor(c(1,0,1)), as.factor(c(0, 0, 1)), mode = "everything"))
})