## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: Create a Matrix capable of caching the result
## of function "solve()"

makeCacheMatrix <- function(x = matrix()) { 
      ## Initialize the result "m" with NULL
      m <- NULL
      
      ## The "set" function assign the value of the parameter "y" 
      ## to the Matrix "x" and then re-initialize the value of "m"
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      ## The "get" function return the value of the Matrix "x"
      get <- function() x
      
      ## "setsolve" assign the result of the solve function to "m"
      ## (we suppose that "solve" is the inverse of the matrix)
      setsolve <- function(solve) m <<- solve
      
      ## "getsolve" return the "m"-value
      getsolve <- function() m
      
      ## creation of the list
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## Write a short comment describing this function
## cacheSolve: Calculate the inverse ("solve()"), checking first if that result 
## was calculated and cached

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      m <- x$getsolve()
      ## If the last value of "m" is not null then show it
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      ## If is null then get "x" and calculate its inverse with solve()
      data <- x$get()
      m <- solve(data, ...)
      
      ## Add this new calculated data to the cache
      x$setsolve(m)
      
      message("getting newly added data")
      m
}
