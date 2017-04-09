## Programming Assignment 2: Lexical Scoping
## Caching the Inverse of a Matrix

## makeCacheMatrix greates four functions, set(), get(), setinv() and getinv()
## and returns the functions within a list to the parent environment
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #sets 'i' to NULL
  set <- function(y) {
    x <<- y #sets the input argument to 'x' in the parent environment
    i <<- NULL #sets 'i' to Null in the parent environment
  }
  get <- function() x #gets the matrix assigned to 'x'
  setinv <- function(inv) i <<- inv #sets the inverse of the matrix to 'i' 
  getinv <- function() i #gets the inverse of the matrix 'i' 
  list(set = set,  # name the set() function as 'set'
       get = get,  # name the get() function as 'get' 
       setinv = setinv, # name the setinv() as 'setinv' 
       getinv = getinv) # name the getinv() as 'getinv'
}


## cacheSolve retrieves or calculates and stores the inverse of the matrix 'x'
cacheSolve <- function(x, ...) {
  i <- x$getinv() #retrieves the cached inverse of the matrix and sets to 'i'
  if(!is.null(i)) { #if 'i' is not NULL
    message("getting cached data") #prints the message
    return(i) #returns 'i'
  }
  #if 'i' is NULL
  data <- x$get() #gets 'x' from the parent environment and set 'data' to 'x'
  i <- solve(data, ...) #generates the inverse of matrix 'data' and set to 'i'
  x$setinv(i) #stores 'i', the inverse of matrix 'x'
  i ## Returns 'i', a matrix that is the inverse of 'x'
}
