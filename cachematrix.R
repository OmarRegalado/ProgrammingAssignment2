makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(m) {
        mtx <- m;
        inverse <- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        message("Browsing previous matrices...")
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)
}
