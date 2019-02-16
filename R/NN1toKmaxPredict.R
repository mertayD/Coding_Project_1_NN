#' NN1toKmaxPredict Algo
#'
#'Coding Project 1 R function for NN1toKmaxPredict Algo
#' @param X.mat numeric input feature matrix [n x p]
#' @param Y.vec numeric input label vetor [n], either all 0/1 for bninary classification or other real numbers for regression, it doesn't support others
#' @param testX.mat numeric test matrix
#' @param max_neighbors scalar integer, max number of neighbors
#'
#' @return numeric vector size of n x max_neighbors, predictions from 1 to max_neighbors for all rows of testX.mat
#' @export
#'
#' @examples
#' data(zip.train, package = "ElemStatLearn")
#' io1 <- which(zip.train[,1] %in% c(0,1))
#' train.i <- io1[1:5]
#' test.i <- io1[6:10]
#' X <- zip.train[train.i, -1]
#' Y <- zip.train[train.i, 1]
#' testX <- zip.train[test.i, -1]
#' max_neighbors <- 3
#' NN1toKmaxPredict(X,Y,testX, 3)
#' 
#' data(prostate, package = "ElemStatLearn")
#' io1 <- which(zip.train[,1] %in% c(0,1))
#' train.i <- io1[1:5]
#' test.i <- io1[6:10]
#' X <- zip.train[train.i, -1]
#' Y <- zip.train[train.i, 1]
#' testX <- zip.train[test.i, -1]
#' max_neighbors <- 3
#' NN1toKmaxPredict(X,Y,testX, 3)

NN1toKmaxPredict <- function(X.mat, Y.vec, testX.mat, max_neighbors){
  result.list <- .C("NN1toKmaxPredict_interface", as.double(X.mat), as.double(Y.vec), as.double(testX.mat), 
                    as.integer(nrow(testX.mat)), as.integer(nrow(X.mat)), as.integer(ncol(X.mat)), 
                    as.integer(max_neighbors),
                    predictions= double(max_neighbors * as.integer(nrow(testX.mat))) , PACKAGE="codungProject1")
  result.list$predictions
}


