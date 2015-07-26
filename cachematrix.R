## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     ##initialize the xinv element to NULL any time a special matrix object is created
     xinv <- NULL
     ##the set function allows the option to set the matrix in the previously defined special matrix object. 
     ## note the use of the <<- operator to access the enviroment where the special matrix object was created
     set <- function(y) {
          x <<- y
          xinv <<- NULL
     }
     ##The get fuction returns the matrix stored in the special matrix object
     get <- function() x
     ## The setinv fucntion allows to set the inverse of the matrix in the special matrix object. Note the use of <<-
     setinv <- function(inv) xinv <<- inv
     ## The getinv function returns the inverse of the matrix stored in the special matrix object
     getinv <- function() xinv
     ##The makeCacheMatrix function returns a list of functions 
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     ##Cachesolve take the special matrix object defined by makeCacheMatrix as input
     ## It calls the value of the inverse stored in the x
     xinv <- x$getinv()
     ## If the inverse called above is not null, then a message is returned along with the matrix inverse
     if(!is.null(xinv)) {
          message("getting cached matrix inverse")
          return(xinv)
     }
     ## For uncached matrices, the data function pulls the matrix stored in X
     data <- x$get()
     ## The matrix inverse in obtained by the solve function & stored in matrix object by the use of setinv()
     xinv <- solve(data, ...)
     x$setinv(xinv)
     #calculated matrix inverse is returned
     xinv
}
