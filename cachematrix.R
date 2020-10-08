# Creation of a special "matrix" object
# containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

# The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # define the cache m
  invs <- NULL
 
   set <- function(y) {
          x <<- y 
          invs <<- NULL 
  }
  
   get <- function() x # returns the matrix x
  
  setinverse <- function(inverse) invs <<- inverse 
  
  getinverse <- function() invs # returns the cached inverse of matrix "x"
  
  list(set = set, 
       get = get,
       
       setinverse = setinverse,
       getinverse = getinverse)
}


# The function computes the inverse of a matrix above
# The result is saved to cache such that the next time the user attempts to calculate the
# matrix inverse, the previously saved value is returned instead of
# repeating the calculation.

cacheSolve <- function(x, ...) {
  # computes and returns  inverse of matrix "x"
  
  invs <- x$getinverse()
     
       if(!is.null(invs)) {
         
         message("getting cached data")
          
           return(invs)
  }
  
  matrixdata <- x$get()
  invs <- solve(data, ...)
  x$setinverse(invs)
   
  invs
}
