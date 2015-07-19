#Test for cache matrix

source('../testsuite.R')

cacheMatrixTestSuite <- function() {
  
  # test
  sampleMatrix <- function() {
    v <- sample(1:10000, 81)
    matrix(v, nrow=9)
  }
  
  test.makeCacheMatrix <- function() {
    # load the functions under test
    source('cachematrix.R')
    input <- sampleMatrix()
    x <- makeCacheMatrix(input)
    assertVectorEquals(x$get(), input)  
  }
  
  list(test.makeCacheMatrix)
}

runTestSuite(cacheMatrixTestSuite)
