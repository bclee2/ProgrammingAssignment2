## This first function creates a list that contains four functions that are used to set values of variables
## and the values of the matrix "x".
## They are used in conjunction with the cacheSolve function, which either calculates the inverse of
## the matrix x or gets the inverse that has previously been calculated.

## The set function can be used to change the values in the matrix as well as nullify the cached
## inverse of the matrix.  This function isn't directly used in the cacheSolve function.

## The get function returns the matrix.  The get function is also used in the cacheSolve function
## to calculate the inverse

## The setinverse function is used to store the resulting inverse calc in the cacheSolve function
## to the variable i (the cache)

##The getinverse function simply returns the cached inverse if there is one

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##The function first retrieves any cached inverse from the makeCacheMatrix function.
##Next the function checks if the cached inverse exists or if it is null.
##If it is not null it returns the inverse along with a message
##If the inverse variable is null the function first calls the matrix using the get function.
##Next it sets i to the inverse matrix.
##Finally it caches the inverse with the setinverse function and prints the inverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
