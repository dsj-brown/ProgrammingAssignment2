## ----------------------------------------------------------------
## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse. 
## ----------------------------------------------------------------

## Pass a list into the function makeCacheMatrix() and assign to a local object
## e.g. myMatrix <- makeCacheMatrix(c(5/8, -1/8, -7/8, 3/8))
## 'm1' and 'm2' are inputs
## 'im' is the  

makeCacheMatrix <- function(m1 = numeric()) {
        m1 <- matrix(unlist(m1), nrow = sqrt(length(m1)))
        im <- NULL
        set <- function(m2) {
                m1 <<- m2
                im <<- NULL
        }
        get <- function() m1
        setInverse <- function(solve) im <<- solve
        getInverse <- function() im
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## ----------------------------------------------------------
## cacheSolve
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## ----------------------------------------------------------
 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## Pass in the name of your local object to return its inverse
## e.g. cacheSolve(myMatrix)

cacheSolve <- function(myMatrix, ...) {
        im <- myMatrix$getInverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        useMe <- myMatrix$get()
        im <- solve(useMe, ...)
        myMatrix$setInverse(im)
        im
}