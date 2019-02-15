#write tests for that R function, in tests/testthat/test-NNLearnCV.R:
#(1) for valid inputs including a user-specified fold.vec your function returns a list.
#(2) the predict function returns a numeric vector of the expected size.
#(3) for an invalid input, your function stops with an informative error message.

library(nearestNeighborsDemo)
library(testthat)
context("test-NNLearnCV")


test_that("NNLearnCV computes same answer as R", {
  data(spam, package = "ElemStatLearn")
  io1 <- which(spam[,1] %in% c(0,1))
  train.i <- io1[1:5]
  test.i <- io1[6]
  x<-spam[train.i, -1]
  y<-spam[train.i, 1]
  fold.vec <- spam[test.i, -1]
  NNLearnCV(x,y,3,fold.vec,5)
  spam[test.i,1]
})

