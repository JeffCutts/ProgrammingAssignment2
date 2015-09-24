# Matrix inversion is usually a costly computation and there may be some benefit to caching the 
# inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix 
# inversion that we will not discuss here). Your assignment is to write a pair of functions that 
# cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

# 1. sets the value of a matrix
# 2. gets the value of a matrix
# 3. sets the value of the inverse of the matrix
# 4. gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL                                      # set value to NULL
        set <- function(y){                                  #1
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x                                  #2
        setinverse <- function(inverse) inv_mat <<- inverse  #3
        getinverse <- function() inv_mat                     #4
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
# not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'
# Note : Assume that the matrix supplied is always invertible
        
        inv_mat <- x$getinverse()
        
        if(!is.null(inv_mat)) {
                message("Getting cached data.")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data)
        x$setinverse(inv_mat)
        inv_mat
        
}

# TEST RESULTS

# > data = rbind(c(11, 22), c(33, 44))
# > testmatrix = makeCacheMatrix(data)
# > testmatrix$get()
# [,1] [,2]
# [1,]   11   22
# [2,]   33   44
# > cacheSolve(testmatrix)   **** 1ST RUN
# [,1]        [,2]
# [1,] -0.1818182  0.09090909
# [2,]  0.1363636 -0.04545455
# > cacheSolve(testmatrix)
# Getting cached data.       **** 2ND RUN
# [,1]        [,2]
# [1,] -0.1818182  0.09090909
# [2,]  0.1363636 -0.04545455
