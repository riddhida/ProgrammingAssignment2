## makeCacheMatrix takes matrix as input and return
## list which contain function(returns matrix) that we want to make as public.


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInverse<-function(solve) m<<- solve
  getInverse<-function() m
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)

}


## cacheSolve function take makeCacheMatrix function output as input and return  
## inverse matrix.

cacheSolve <- function(x, ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setInverse(m)
  m
}
