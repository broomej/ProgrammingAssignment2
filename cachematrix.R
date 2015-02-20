## Defines two functions, "makeCacheMatrix" and "cacheSolve", which define a
## series of functions to calculate and cache the inverse of a matrix, and to call
## those functions as needed, respectively

## Creates a list which contains 4 functions which can be called by name
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## Sets the value of m as NULL
  
  ## Defines the function "set", which takes argument "y", and defines "x" in
  ## the parent environement as "y", and "m" as NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Defines the function "get", which returns the value of X
  get <- function() x 
    
  ## Defines the function "setsolve", which defines "m" in the parent environment
  setsolve <- function(slv) m <<- slv
  
  ## Defines the function "getsolve", which returns m
  getsolve <- function() m
  
  ## Returns a list with 4 elements, which are functions that can be called
  ## by name
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## calls the "getsolve" function, and assigns the returned value to "m"
  m <- x$getsolve()
  
  ## If "m" is NULL, it will throw the message, then return the cached matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## calls the "get" function, and assigns the returned value to "data"
  data <- x$get() 
  m <- solve(data, ...) #calculates the inverse of data and assigns it to "m"
  x$setsolve(m) #calls the "setsolve" function on "m" (i.e. caches the inverse)
  m ## returns the value of "m"
}
