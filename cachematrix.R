##The R Function caches potentially time-consuming computations.  Matrix inversion is also a costly computation.
##The pair of functions makeCacheMatrix and cacheSolve cache the inverse of a matrix.  


##makeCacheMatrix creates a special matrix and contains function to
## 1. set the value of the matrix  2. get the value of the matrix
## 3. set the value of the inverse 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()){
        invMat <- NULL
        set <- function(y){
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setinvMat <- function(inverse) invMat <<- inverse
        getinvMat <- function() invMat
        list(set = set, get = get,
             setinvMat = setinvMat,
             getinvMat = getinvMat)
}

##cacheSolve function calculates the inverse of the special matrix created with makeCacheMatrix function
##the function checks if the inverse has already been calculated - if yes it gets the inverse from the cache and skips the computation
##else it calculates the inverse and sets the inverse in the cache via setinvMat function

cacheSolve <- function(x,...){
##Return a matrix that is the inverse of 'x'
        invMat <- x$getinvMat()
        if(!is.null(invMat)){
                message("Getting cached data")
                return(invMat)
        }
        data <- x$get()
        invMat <- matrix(data,...)
        x$setinvMat(invMat)
        invMat
}
