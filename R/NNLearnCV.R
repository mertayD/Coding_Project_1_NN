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
#'    NNLearnCV(x, y, max.neighbors, fold.vec, n.folds)
#'    
#'    data(spam, package = "ElemStatLearn")
#'    x<-spam[1:5, 1]
#'    y<-spam[1:5, 1]
#'    max.neighbors <- 30
#'    fold.vec <- NULL
#'    n.folds <- 5
#' 
#' 
NNLearnCV <- function(X.mat, y.vec, max.neighbors=30,
                      fold.vec=NULL, n.folds=5) 
{
  # Type Flag
  FLAG = "binary"
  
  #if fold.vec is null randomly assign folds
  if(is.null(fold.vec))
  {
    fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
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
  
  # perform cross-validation to compute two matrices 
  # of mean loss values
  
  # If the labels (y.vec) are all in {0,1} then the 
  # loss function should be the 01-loss (binary 
  # classification), otherwise use the square loss (regression).
  
  # run NN1toKmaxPredict to compute predictions, 
  # then a matrix of loss values then use colMeans() 
  # and store the result in one column of 
  # train.loss.mat/validation.loss.mat.
  
  # k predictions list
  k.predictions <- matrix()
  
  # loop through folds
  for(fold.i in fold.vec)
  {
    # get sets for folds
    train.set <- fold.vec[fold.i]
    # train.set.label <- n.folds[fold.i]
    validation.set <- n.folds.remove(fold.i)
    
    # loop through validation set
    for(valid.i in validation.set)
    {
      # loop through knn
      for(kValue.i in max.neighbors)
      { 
        if(IDK != 1 || IDK != 0)
        {
          FLAG = "reg"
        }
        
        k.predictions[kValue.i] <- NN1toKmaxPredict(train.set, IDK, validation.set, k.predictions)
      }
      
      # binary or regression
      if(FLAG == "binary")
      {
        loss.mat[valid.i] <- ifelse(k.predictions>0.5, 1, 0) != y.vec #zero-one loss for binary classification.
      }
      else
      {
        loss.mat[valid.i] <- (k.predictions-y.vec)^2 #square loss for regression.
      }
      
      #train.or.validation.loss.mat[, fold.i] <- colMeans(loss.mat) #???
      
    }
    
  }
  
  
  # return a list with the following named elements:
  # X.mat, y.vec: training data.
  # train.loss.mat, validation.loss.mat (matrices of loss values for each fold and number of neighbors).
  # train.loss.vec, validation.loss.vec (vectors with max.neighbors elements: mean loss over all folds).
  # selected.neighbors (number of neighbors selected by minimizing the mean validation loss).
  # predict(testX.mat), a function that takes a matrix of inputs/features and returns a vector of predictions.
  returnList <- list("X.mat" = X.mat, "y.vec" = y.vec, "train.loss.mat" = train.loss.mat,
                     "validation.loss.mat" = validation.loss.mat, "train.loss.vec" = train.loss.vec, 
                     "validation.loss.vec" = validation.loss.vec, "selected.neighbors" = selected.neighbors, 
                     "predict(testX.mat)" = predict(testX.mat))
  
  }


