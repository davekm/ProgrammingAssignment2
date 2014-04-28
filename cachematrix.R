## These functions cache a matrix and calculate the inverse of that matrix

## This function caches the matrix, x

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y) {
	x <<- y
	i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setmean=setmean, getmean=getmean)

}


## This function calculates the inverse of the matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i = x$getinv()
	if(!is.null(i)) {
	    message("getting cached data")
	    return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i
}
