#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has #already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
inverse_matrix <- NULL  
        set <- function(j) {    
                x <<- j    
                inverse_matrix <<- NULL  }  
        get <- function() x  
        setInverse <- function(inverse) 
                inverse_matrix <<- inverse  
        getInverse <- function() inverse_matrix  
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)}
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has #already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inverse_matrix <- x$getInverse()  
        if (!is.null(inverse_matrix)) {    
                return(inverse_matrix)  
        }  
        solve_matrix <- x$get()  
        inverse_matrix <- solve(solve_matrix, ...)  
        x$setInverse(inverse_matrix)  
        inverse_matrix
}
