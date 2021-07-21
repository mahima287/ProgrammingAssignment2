##makeCacheMatrix function will take input as a matrix and has some predefined functions called get,setInverse and getInverse

##cacheSolve-is used to compute the inverse of passed matrix.


makeCacheMatrix<-function(x=matrix()){
      inv<- NULL
      set<-function(y){
        x<<-y
        inv<<-NULL
      }
      get <- function()  {x}
      setInverse <-function(inverse) {inv <<- inverse}
      getInverse <-function() {inv}
      list(set= set, get = get ,setInverse = setInverse, getInverse=getInverse)
}

cacheSolve<- function(x , ...){
  inv<-x$getInverse()
  if(!is.null(inv)){
      message("getting cached data")
      return(inv)
  }
  mat <-x$get()
  inv <-solve(mat, ...)
  x$setInverse(inv)
  inv
}