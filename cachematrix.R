## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Create a function which takes a matrix named A as an argument
makeCacheMatrix <- function(A = matrix()) {
  
  ## set the initial value of inv
  inv <- NULL
  
  set <- function(B) {
    
    ## assign the value of B to A (stored in the parent environment)
    A <<- B
    
    ## assign NULL in inv (stored in the parent environment)
    inv <<- NULL
  }
  ## return the matrix A
  get <- function() A
  
  ## sets the value of the inversed matrix to the inv variable (stored in the parent environment)
  setinverse <- function(inverse) inv <<- inverse
  
  ## return the cached value of inversed matrix A
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

## Create a function getting matrix A as an argument along with other possible arguments
cacheSolve <- function(A, ...) {
  
  ## assign the value of the cached inverse matrix from cacheMatrix to the inv variable
  inv <- A$getinverse()
  
  ## if the value is different than null (meaning it was computed already), return the value of inversed matrix A
  if(!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  
  ## assign the value of A to a matrix named mymatrix 
  mymatrix <- A$get()
  
  ## store the value of inversed matrix A to inv variable
  inv <- solve(mymatrix)
  
  ## cache the inv variable for further usage and printing inv
  A$setinverse(inv)
  inv
}
