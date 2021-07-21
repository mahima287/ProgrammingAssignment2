##makeCacheMatrix function will take input as a matrix and has some predefined functions called get,setInverse and getInverse

##cacheSolve-is used to compute the inverse of passed matrix.

##library(MASS) is used to calculate inverse for non-square as well for square matrices.
library(MASS)
makeCacheMatrix<-function(x=matrix()){
      inv<- NULL           #initializing inverse as NULL
      set<-function(y){
        x<<-y
        inv<<-NULL
      }
      get <- function()  {x}       #function to get matrix x
      setInverse <-function(inverse) {inv <<- inverse}     #function to obtain inverse of matrix
      getInverse <-function() {
                 inver<- ginv(x)
                 inver%%x
               }
      list(set= set, get = get ,setInverse = setInverse, getInverse=getInverse)
}

cacheSolve<- function(x , ...){     ## gets cached data
  inv<-x$getInverse()
  if(!is.null(inv)){
      message("getting cached data")
      return(inv)                #returns inverse value
  }
  mat <-x$get()
  inv <-solve(mat, ...)
  x$setInverse(inv)
  inv                  # returns a matrix that is inverse matrix of 'x'
}