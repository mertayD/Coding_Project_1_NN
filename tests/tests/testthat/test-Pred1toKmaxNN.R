#(1) for a valid input (one of the data sets mentioned below), your function returns a valid numeric prediction matrix with the expected dimensions, and 
#(2) for an invalid input, your function stops with an informative error message. 
#(3) Extra credit if you compute the predictions for one observation in R code, and write a test that the C++ code computes the same thing.

library(codungProject1)
library(testthat)
context("NN1toKmaxPredict")
test_that("NN1toKmaxPredict computes same answer as R", {
  data(zip.train, package = "ElemStatLearn")
  io1 <- which(zip.train[,1] %in% c(0,1))
  train.i <- io1[1:5]
  test.i <- io1[6:7]
  train_size <- length(train.i)
  test_size <- length(test.i)
  X <- zip.train[train.i, -1]
  Y <- zip.train[train.i, 1]
  testX <- zip.train[test.i, -1]
  max_neighbors <- 4
  pred_vec <- NN1toKmaxPredict(X,Y,testX, max_neighbors)
  
  dist_matrix <- matrix(, nrow = test_size, ncol = train_size)
  sorted_index_mat <- matrix(, nrow = test_size , ncol = train_size)
  expected_pred_mat <- matrix(, nrow = test_size, ncol= max_neighbors)
  
  for(i in 1: nrow(testX) )
  {
    dist_mat <- t(X) - testX[i,]
    dist_matrix[i,] <- sqrt(colSums(dist_mat * dist_mat))
    sorted_index_mat[i, ] <- order(dist_matrix[i,])
    closest_indices <- sorted_index_mat[i, 1:max_neighbors]
    expected_pred_mat[i,] <- cumsum(Y[closest_indices])/(1:max_neighbors)
  }
})

