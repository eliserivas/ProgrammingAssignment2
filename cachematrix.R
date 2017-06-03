## These function together caching the inverse of a matrix 
## rather than compute it repeatedly
## The first creates a matrix object that is calculated
## in the second, checking to see if it has already been computed

# 1. The function below creates the matrix
# 2. gets the matrix
# 3. sets the value of the inverse
# 4. and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) { # Create matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x # Retrieves matrix
  setInverse <- function(inverse) i <<- inverse # Sets the inverse
  getInverse <- function() i # Gets the inverse
  list(set=set, 
       get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)

}


## Calculates the inverse of the matrix created in the function above,
## but first checks to see if it's been calculated already.
## If so, it skips the computation. If not, it calculates the inverse
## using setInverse from the previous function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
