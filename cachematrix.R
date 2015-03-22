
# The function makeCacheMatrix returns a list of following functions
# (setMatrix, getMatrix, cacheMatrixInverse, getMatrixInverse)
# It stores a martix and a cached value of the inverse of the matrix.

makeCacheMatrix <- function(x = numeric()) {

        # initially nothing is cached so cache is set to NULL
        cache <- NULL
        
        # setMatrix assigns the matrix to value x, sets cache to NULL
        setMatrix <- function(newValue) {
                x <<- newValue
                cache <<- NULL
        }
        
        # getMatrix returns the stored matrix
        getMatrix <- function() {
                x
        }
        
        # cacheMatrixInverse assigns matrix inverse to cache
        cacheMatrixInverse <- function(solve) {
                cache <<- solve
        }
        # getMatrixInverse returns the cached value
        getMatrixInverse <- function() {
                cache
        }
        # return a list. Each named element of the list is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheMatrixInverse = cacheMatrixInverse, getMatrixInverse = getMatrixInverse)
}

# cacheSolve function calculates the inverse of a "special" matrix created with
# the function makeCacheMatrix
cacheSolve <- function(y, ...) {
        
        # get the cached value using getInverse function
        matrix_inverse <- y$getMatrixInverse()
        
        # return the cached value if stored
        if(!is.null(matrix_inverse)) {
                message("getting cached data")
                return(matrix_inverse)
        }
        # get the matrix using getMatrix, caclulate the inverse using solve
        # and store it the cache using cacheMAtrixInverse
        data <- y$getMatrix()
        matrix_inverse <- solve(data)
        y$cacheMatrixInverse(matrix_inverse)
        
        # return the inverse
        matrix_inverse
        
}


