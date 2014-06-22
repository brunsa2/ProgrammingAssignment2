#' Matrix with cached inverse. Provides function to create matrix that can cache inverse
#' and function to cache inverse or use cached version
#' @author Jeff Stubler

#' Makes a matrix representation with accessor and mutator pairs for the matrix and
#' for its inverse.
#' 
#' @param x Matrix to create cached version of
#' 
#' @return List of get and set functions for both matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(new_matrix) {
        x <<- new_matrix
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(new_inverse) inverse <<- new_inverse
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#' Solves for the matrix inverse and caches it, or returns the cached version
#' if available.
#' 
#' @param x Cached matrix to calculate inverse of
#' 
#' @return Calculated or cached inverse of matrix
cacheSolve <- function(x, ...) {
    current_inverse <- x$getinverse()
    if (!is.null(current_inverse)) {
        message("Using cached version")
        return(current_inverse)
    } else {
        current_matrix <- x$get()
        calculated_inverse <- solve(current_matrix, ...)
        x$setinverse(calculated_inverse)
        return(calculated_inverse)
    }
}
