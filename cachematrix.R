## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse when a new matrix is set
  }
  
  get <- function() x  # Return the matrix
  setinverse <- function(inverse) inv <<- inverse  # Store the inverse
  getinverse <- function() inv  # Retrieve the inverse
  
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Retrieve cached inverse if available
  
  if (!is.null(inv)) {  # If inverse is already calculated, return it
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse
  x$setinverse(inv)  # Store the inverse in cache
  
  inv  # Return the computed inverse
}


