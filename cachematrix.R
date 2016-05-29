## makeCacheMatrix
## input: a matrix
##return value: a list containing functions

##cacheSolve
##input: a list containing functions
##output: inverse of the matrix, that the user passed to makeCacheMatrix

## The user passes a matrix to makeCacheMatrix,
## varibale x stores the currently passed matrix, varible inverse stores the inverse of the current matrix
## set() assigns the passed matrix to the variable x and sets the inverse to Null
## get() simply returns the current matrix
## setinverse() takes in the inverse of the current matrix and sets it to the variable inverse
## getinverse() fetches the value stored in inverse
## finally all the above functions are returned in the form of a list
makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
   set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(y) inverse <<-y
  getinverse <-function() inverse
  list(set = set, get = get,
       getinverse= getinverse ,
       setinverse= setinverse)
}

## the function cachesolve
## it first checks the value of inverse, if the inv is null that is, the inv hasn't been calculated
## it calculates the inverse (inverse <- solve(mat, ...)) and then returns the inverse 
## if the inverse is not null that is, it has already been calculated it returns the cached value
## thus avoiding the recalculation if the original matrix has not changed

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
