## Much of what occurs below is patterned after the example functions, with a bit of
## tweaking in order to fit the needs of the description. Further, the simplest 
## situations are assumed here, i.e., no "outlier" situations. 

## Works the same way MakeVector does. Since we are allowed to assume only invertible
## matrices, this function only works with SQUARE matrices.

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix() #similar to m; null-matrix initialization of inverse.
  get<- function() x #takes the input matrix; assumes it to be square
  set<- function(y) {
    x<<-y # changes the contents of the given matrix
    inv <- NULL # resets inverse
  }
  setinv<-function(newinv=matrix()) inv<<-newinv #manually sets the new inverse
  getinv<-function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Works similarly as cacheMean, but reworked the details for the solving of inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  tempinv<-x[[getinv()]]
  if(!is.na(tempinv)){
      message("getting cached data")
      return(tempinv)
  }
  dat<- x$get()
  newinv<-solve(dat)
  x$setinv(newinv)
  newinv
}

## The code should work for well for a good tolerance level (at around 10exp(-13))