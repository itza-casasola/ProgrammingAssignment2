## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #assigns NULL to an 'inv' variable within the current environment
        set <- function(y) {
                x <<- y #cache the matrix
                inv <<- NULL
        }
        get <- function() x #Gets the value of the cached array with set
        setinv <- function(invMatrix) inv <<- invMatrix #The cached value of the inverse matrix is stored in inv
        getinv <- function() inv #Gets the saved value of the inverse array inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) #list with the 4 functions already defined
        #function output
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinv() #See if the inverse has already been created
        if(!is.null(inv)) { #check if 'cacheSolve' has been executed before
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get() #gets the value of the input array
        inv <- solve(matrix, ...) #computes the inverse of the input matrix
        x$setinv(inv) #cache the inverse
        inv
        ## Return a matrix that is the inverse of 'x'
}

