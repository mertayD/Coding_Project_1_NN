
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
#'    max.neighbors <- 5
#'    n.folds <- 5
#'    fold.vec <- sample(rep(1:n.folds, l=nrow(x)))
#'   returned <- NNLearnCV(X.mat, y.vec, max.neighbors, fold.vec, n.folds)
#' 
NNLearnCV <- function(X.mat, y.vec, max.neighbors=30,
                      fold.vec=NULL, n.folds=5) {

  validation.loss.mat = matrix(, nrow = n.folds, ncol = max_neighbors)
  train.loss.mat = matrix(, nrow = n.folds, ncol = max_neighbors)
  
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
    
    pred_mat <- matrix(pred_vec_val,nrow = n_rows_validation_set ,ncol = max_neighbors)
    loss <- matrix((pred.mat - validation_labels)^2,nrow = n_rows_validation_set ,ncol = max_neighbors)
    
    validation.loss.mat[fold.i ,] <- colMeans(loss) #square loss for regression.
    
    pred_vec_train <- NN1toKmaxPredict(
      train_set, train_labels,
      train_set, max.neighbors)
    
    pred_mat <- matrix(pred_vec_train ,nrow = n_rows_train_set,ncol = max_neighbors)
    loss <- matrix((pred.mat - train_labels)^2,nrow = n_rows_validation_set ,ncol = max_neighbors)
    train.loss.mat[fold.i,] < colMeans(loss)
  }
  
  # return a list with the following named elements:
  # X.mat, y.vec: training data.
  # train.loss.mat, validation.loss.mat (matrices of loss values for each fold and number of neighbors).
  # train.loss.vec, validation.loss.vec (vectors with max.neighbors elements: mean loss over all folds).
  # selected.neighbors (number of neighbors selected by minimizing the mean validation loss).
  # predict(testX.mat), a function that takes a matrix of inputs/features and returns a vector of predictions.
  
  returnList <- list("X.mat" = X.mat, "y.vec" = y.vec, "train.loss.mat" = train.loss.mat,
                     "validation.loss.mat" = validation.loss.mat)
 returnList$train.loss.mat
}

