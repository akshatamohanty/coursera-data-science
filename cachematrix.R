## Programming Assignment 2 - R Programming
## Usage :
## test_matrix <- matrix(c(1,0,5, 2, 1, 6, 3, 4, 0), nrow=3, ncol=3)
## cacheSolve(special_matrix)
## [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## cacheSolve(special_matrix)
## Getting cached data
## [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## 
## Resetting the matrix to a new matrix
## new_matrix <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow=3, ncol=3)
## special_matrix$set(new_matrix)
## cacheSolve(special_matrix)
## [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1
## cacheSolve(special_matrix)
## Getting cached data...
## [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1

## This function creates a special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
      # Inverse of the matrix is initially set to NULL  
      inv <- NULL
      
      # Set function resets the value of the matrix and clears cached values
      set <- function(y){
          x <<- y
          inv <<- NULL
      }  
      
      # Get function gets the value of the matrix
      get <- function() x
      
      # Set Inverse function sets the inverse of the matrix
      setInv <- function(inverse)   inv <<- inverse
      
      # Get Inverse function gets the inverse of the matrix if it exists
      getInv <- function()  inv
      
      list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## The function computes the inverse of the matrix if not already cached else displays
## the cached value

cacheSolve <- function(x, ...) {
        
        # Fetches the inverse value of the matrix
        inv <- x$getInv()
        
        # If the inverse value of the matrix exists in the cache, the cached value is fetched
        # and function exits
        if(!is.null(inv)) {
          message("Getting cached data...")
          return(inv)
        }
        
        # If the inverse doesn't exist already, it is computed, cached and displayed
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
