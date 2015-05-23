## makeCacheMatrix: This function creates a list of the following functions:
## Set: caches a matrix
## Get: calls cached matrix 
## Setinv: estimates the inverse of the matrix in the argument and caches it
## Getinv: calls the cached inverse of the matrix
## Required package: matrixcalc (for singularity test)

library(matrixcalc)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y             #Cache matrix in environment
                inv <<- NULL        #Clear inverse of the matrix from cache
        }
        get <- function() {
                x                   #Call matrix from cache
        }
        setinv <- function(x) {
                if (ncol(x) == nrow(x)) {                 #Test for equal dimensions
                        if (!is.singular.matrix(x)) {     #Test for determinant not equal to 0
                                inv <<- solve(x)          #Solve for inverse of the matrix and cache it   
                        } else {
                                stop("Matrix is singular")
                        }   
                } else {
                        stop("Unequal matrix dimensions")
                }
        }
        getinv <- function() {
                inv                #Call inverse of the matrix from cache
        }
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve: This function returns the invesre of a matrix from the cache, 
## if cache is empty it calculates the inverse of the matrix.
## Required package: matrixcalc (for singularity test)

cacheSolve <- function(x, ...) {
        inv <- x$getinv()                     #Call inverse of matrix
        if (is.null(inv)) {                   #If inverse of matrix doesn't exist...
                data <- x$get()               #Call matrix
                x$setinv(data)                #Calculate inverse of matrix, store in cache
                inv <- x$getinv()             #Pull inverse of matrix from cache for display
                return(inv)                   #Display inverse of the matrix
        } else {
                message("Getting cached data")
                return(inv)                   #Display inverse of the matrix
        }
}