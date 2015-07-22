## The following set of functions create, cache, and retrieve the inverse of a matrix.

## "makeCacheMatrix" creates a matrix and caches it's inverse.
makeCacheMatrix <- function(x = matrix()) {
     cmat <- NULL
     set <- function(a){
          x <<- a
          cmatrix <<- NULL
     }
     get <- function() x
     setmat <- function(inverse) cmat <<- inverse
     getmat <- function () cmat
     list(set = set, get = get, setmat = setmat, getmat = getmat)
}


## "cacheSolve" returns the inverse of "makeCacheMatrix".  If "makeCacheMatrix" is null, it will create one.
cacheSolve <- function(x, ...) {
     cmat <- x$getmat()
     if (!is.null(cmat)){
          message("Retrieving cached matrix")
          return(cmat)
     }
     ## Returns the inverse of the cached matrix if it is not null.
     else {
          nmat <- x$get()
          cmat <- solve(nmat, ...)
          x$setmat(cmat)
          return(cmat)
     }
     ## Creates, caches, and returns the inverse of the matrix, if it was previously null.
}
