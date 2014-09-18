# makeCacheMatrix and cacheSolve equations for rprogramming assignment2

# Make a invertable matrix

# makeCacheMatrix creates a list of functions that can act on the matrix 
# provided to makeCacheMatrix these functions are used by the cacheSolve 
# function as it calculates and caches the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
      
      i <- NULL                           # Assigns NULL value to variable:i; 
                                          # used in cacheSolve function below.
      
      set <- function(y) {                # set function allows us to update the 
                                          # matrix without rerunning makeCacheMatrix 
                                          # (not strictly necessary for our needs 
                                          # but preserved for consistency).
            
            x <<- y                       # Updates variable:x to the new matrix 
                                          # supplied to the set function.
            
            i <<- NULL                    # Updates variable:i to NULL; prevents 
                                          # reusing an out-of-date cached inverse.
      }
      
      get <- function() x                 # get function returns the current matrix.
      
      setinv <- function(inv) i <<- inv   # setinv function updates the cached 
                                          # value of the matrix inverse.
      
      getinv <- function() i              # getinv function returns the cached 
                                          # value of the matrix inverse; cached
                                          # value is NULL until calculated 
                                          # in cacheSolve below.
      
      list(set = set, get = get,          # Creates a list of function names 
           setinv = setinv,               # and functions for use by the 
           getinv = getinv)               # cacheSolve function below.
}

# cacheSolve returns the cached matrix inverse or calculates and caches the matrix 
# inverse using the list of functions defined by the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {            
      
      i <- x$getinv()                     # Assigns variable:i using the getinv 
                                          # function from makeCacheMatrix function; 
                                          # getinv function should return NULL for 
                                          # first run of cacheSolve().
      
      if(!is.null(i)) {                   # Checks to see if current value of 
                                          # variable:i is NULL; if NULL then exit
                                          # if statement and calculate matrix inverse. 
      
            message("getting inverse")    # Print message "getting inverse". 
      
            return(i)                     # Prints matrix inverse from cache.
      }                                   # End if statement.

      data <- x$get()                     # Assigns current matrix to variable:data 
                                          # using the get function from makeCacheMatrix 
                                          # function; get function should return the 
                                          # current matrix.
      
      i <- solve(data, ...)               # assigns variable:i the inverse of 
                                          # the current matrix
      
      x$setinv(i)                         # uses the setinv function from makeCacheMatrix 
                                          # to update the cached matrix inverse in the
                                          # makeCacheMatrix environment from NULL to the 
                                          # calculated inverse; allows future runs of 
                                          # cacheSolve to return the cached inverse 
                                          # instead of re-calculaating.
      
      i                                   # returns the calculated matrix inverse
}