## Given a Matrix x, it creates and returns a CacheMatrix object.
## The available methods are: set, get, setmean, getmean, setim, getim.

makeCacheMatrix <- function(x = matrix()) {
    makeVector(x)
}


## This function receives a CacheMatrix X as parameter and checks if the inverted matrix is already calculated.
## If the inverted matrix is not calculated yet, this function calculates it and put it on cache.
## If the inverted matrix is already calculated on cache, this function only returns it.
## Returns the x inverted matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cacheim(x, ...)
}


## PRIVATE FUNCTIONS START ##

## Given a Matrix x, it creates and returns a CacheMatrix object.
## The available methods are: set, get, setmean, getmean, setim, getim.

makeVector <- function(x = numeric()) {
    m <- NULL
    inv_m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        inv_m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    setim <- function(inv_matrix) inv_m <<- inv_matrix
    getim <- function() inv_m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean,
         setim = setim,
         getim = getim)
}

## Returns the mean of CachedMatrix object.
## If the mean is not cached yet, this function calculates and caches it.

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting mean cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

## Returns the inverted matrix of CachedMatrix object.
## If the inverted matrix is not cached yet, this function calculates and caches it.

cacheim <- function(x, ...) {
    inv_m <- x$getim()
    if(!is.null(inv_m)) {
        message("getting inverted matrix cached data")
        return(inv_m)
    }
    data <- x$get()
    inv_m <- solve(data, ...)
    x$setim(inv_m)
    inv_m
}

## PRIVATE FUNCTIONS END ##