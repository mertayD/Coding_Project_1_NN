
#' Nearest Neighbors Learning Algorithm
#'
#'   This is a learning algorithm that uses cross-validation 
#'   to select the number of neighbors that minimizes the mean
#'   validation loss
#'
#' @param X.mat a training data set
#' @param y.vec a training data set
#' @param max.neighbors=30 The max number of neighbors to fin the best k
#'                         in nearest neighbors
#' @param fold.vec=NULL is a vector of fold ID numbers. If fold.vec is NULL
#'                      randomly assign fold IDs from 1 to n.folds
#' @param n.folds=5 is the number of folds used to compute error
#'
#' @return returnList a list containing:
#'         X.mat - training data
#'         y.vec - training data
#'         train.loss.mat - matrice of loss values for each fold and number
#'           of neighbors
#'         validation.loss.mat - matrice of loss values for each fold and
#'            number of neighbors
#'         train.loss.vec - vector with max.neighbors elements: 
#'            mean loss over all folds
#'         validation.loss.vec - vector with max.neighbors elements: 
#'            mean loss over all folds
#'         selected.neighbors - number of neighbors selected by minimizing 
#'            the mean validation loss
#'         predict(testX.mat) - a function that takes a matrix of 
#'            inputs/features and returns a vector of predictions. 
#'            It should check the type/dimension of testX.mat and stop() 
#'            with an informative error message if there are any issues.
#'         
#' @export
#' 
#' @examples
#'    library(codungProject1)
#'    
#'    data(zip.train, package = "ElemStatLearn")
#'    X.mat<-zip.train[1:50,-1]
#'    y.vec<-zip.train[1:50, 1]
#'    max.neighbors <- 30
#'    n.folds <- 7
#'    fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
#'    
#'    result <- NNLearnCV(X.mat, y.vec, max.neighbors, fold.vec, n.folds)
#' 
NNLearnCV <- function(X.mat, y.vec, max.neighbors=30,
                      fold.vec=NULL, n.folds=5) {

  
  #if fold.vec is null randomly assign folds
  if(is.null(fold.vec))
  {
    fold.vec <- sample(rep(1:n.folds, l=nrow(x)))
  }
  
  # make sure that fold.vec is the same size as y.vec
  # which is the same as the number of rows in X.mat
  if(nrow(X.mat) != length(y.vec) &&  
     nrow(X.mat) != length(fold.vec) &&
     length(fold.vec) != length(y.vec))
  {
    stop("y.vec, fold.vec, and X.mat columns are not equal.
         Program could not complete.")
  }
  
  validation.loss.mat <- matrix(, nrow = n.folds, ncol = max.neighbors)
  train.loss.mat <- matrix(, nrow = n.folds, ncol = max.neighbors)
  
  for(fold.i in 1:n.folds){
    validation_indices <- which(fold.vec %in% c(fold.i))
    validation_set <- X.mat[validation_indices,]
    train_set <- X.mat[-validation_indices,]
    train_labels <- y.vec[-validation_indices]
    validation_labels <- y.vec[validation_indices] 
    n_rows_validation_set <- nrow(validation_set)
    n_rows_train_set <- nrow(train_set)
    # predict using train_set and validation_set
    pred_vec_val <- NN1toKmaxPredict(
        train_set, train_labels,
        validation_set, max.neighbors)
    
    pred_mat_val <- matrix(pred_vec_val,nrow = n_rows_validation_set ,ncol = max.neighbors)
    value_val <- (pred_mat_val - validation_labels)^2
    loss <- matrix(value_val ,nrow = n_rows_validation_set ,ncol = max.neighbors)
    
    validation.loss.mat[fold.i,] = colMeans(loss) #square loss for regression.
    
    
    pred_vec_train <- NN1toKmaxPredict(
      train_set, train_labels,
      train_set, max.neighbors)
    
    pred_mat_train <- matrix(pred_vec_train ,nrow = n_rows_train_set,ncol = max.neighbors)
    value_train <- (pred_mat_train - train_labels)^2
    loss <- matrix(value_train ,nrow = n_rows_validation_set ,ncol =max.neighbors)
    train.loss.mat[fold.i,] = colMeans(loss)
  }

  validation.loss.vec <- colMeans(validation.loss.mat)
  train.loss.vec <- colMeans(train.loss.mat)
  
  selected.neighbors <- which.min(validation.loss.vec)
  
  
  
  # return a list with the following named elements:
  # X.mat, y.vec: training data.
  # train.loss.mat, validation.loss.mat (matrices of loss values for each fold and number of neighbors).
  # train.loss.vec, validation.loss.vec (vectors with max.neighbors elements: mean loss over all folds).
  # selected.neighbors (number of neighbors selected by minimizing the mean validation loss).
  # predict(testX.mat), a function that takes a matrix of inputs/features and returns a vector of predictions.
  
  result.list <- list("X.mat" = X.mat, "y.vec" = y.vec, "train.loss.mat" = train.loss.mat,
                     "validation.loss.mat" = validation.loss.mat, "train.loss.vec" = train.loss.vec, 
                     "validation.loss.vec" = validation.loss.vec, "selected.neighbors" = selected.neighbors)
}

