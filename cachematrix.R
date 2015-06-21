## 'makeCacheMatrix' assigns the values of matrix and inverse matrix into a list of functions.
## It stores the previous values of inverse matrix as cache.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ##nullifies the previous stored inverse values.
     setm <- function(y) {  ##'setm' function sets new values of the matrix
        x <<-y  ##
        i <<- NULL ##nullifies the previous stored inverse, as new matrix is assigned.
     }
     getm <- function() x   ##'getm' function print value of the matrix
     setinv <- function(inv) i <<- inv  ##'setinv' function sets new values of the inverse
     getinv <- function() i        ##'getinv' function prints value of the inverse
     list(setm = setm, getm = getm, setinv = setinv, getinv = getinv) ##forms list of functions
}

## 'cacheSolve' returns a matrix that is inverse of the input matrix.
## It checks if the inverse is already calculated and if true, it takes data from cache.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
     i <- x$getinv()
     if(!is.null(i)) {  ##checks if the 'i' is NULL or not. NULL indicates new value is assigned.
       message("getting cached data")
       return(i)
     }
     matrix <- x$getm()  ##fetches the value of x into the 'matrix' variable.
     i <- solve(matrix, ...) ##calulates the value of inverse.
     x$setinv(i)
     i                      ## prints the value of inverse matrix.
}