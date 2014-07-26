makeCacheMatrix <-function(x = matrix()){
  
  
  ix <- NULL
  set <- function(y){
    x <<- y
    ix <<- NULL
    
  }
  
  get <- function() x
  setinverse <- function(inverse) ix <<- inverse
  getinverse <- function() ix
  
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
  
}




cacheSolve <- function(x, ...){
  
  ix <- x$getinverse()
  if(!is.null(ix)){
    message("getting cached inverse matrix")
    return(ix)
  }
  matrixdata <- x$get()
  ix <- solve(matrixdata, ...)
  x$setinverse(ix)
  ix
  
}
