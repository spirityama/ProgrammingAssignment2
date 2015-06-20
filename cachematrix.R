## function creates object to cache matrix inverse

makeCacheMatrix <- function(m = matrix()) {
	i <- NULL
	    set <- function(x) {
      	  m <<- x;
	        i <<- NULL;
	    }
	    get <- function() return(m);
	    setinv <- function(inv) i <<- inv;
	    getinv <- function() return(i);
	    return(list(set = set, get = get, setinv = setinv, getinv = getinv))

}


## this function calculates the inverse of matix m returned 
## from above   makeCacheMatrix

cacheSolve <- function(m, ...) {
		i <- m$getinv()
	    if(!is.null(i)) {
      	  message("Getting cached data...")
	        return(i)
	    }
	    data <- m$get()
	    i <- solve(data, ...)
	    m$setinv(i)
	    return(i)
        ## i is the inverse of matrix 
}
