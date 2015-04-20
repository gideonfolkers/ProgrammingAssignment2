## Two Functions below cache the inverse of a matrix
## makeCacheMatrix creates the inverse of the matrix
## cachSolve checks if the inverse of the matrix is cached, otherwise it is calculated

## Function creates the inverse of the matrix x
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize ‘inv_x’ null
        inv_x <- NULL
        
        ## Set the value 
        set <- function(y) {
            x <<- y
            inv_x <<- NULL
          }
        
        ## Get the value
        get <- function() x
        
        ## Set the inversed x with solve function
        setinverse <- function(solve) inv_x <<-inverse
       
        ## Get the inversed x
        getinverse <- function() inv_x
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Check if the inverse of the matrix is cached, return it.
## If the inversed x was not cached, calculate

cacheSolve <- function(x, ...) {
  
        ## Get the value of inversed x
        inv_x <- x$getinverse()
        
        ## If the inversed x was cached, return it.
        if (!is.null(inv_x)) {
          
              message("getting cached data")
              return(inv_x)
        }
        ## If the inversed x was not cached, calculate and return it.
        data <- x$get()
        inv_x <- solve(data)
        x$setinverse(inv_x)
        
        ## Return the inverse matrix of x
        inv_x
}