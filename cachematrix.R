library(testthat)
## Function returning an object containing a matrix. 
##   To get the original matrix, use $get(), to change the matrix use $set
##      The other functions should not be used by external users.
makeCacheMatrix <- function(x = matrix()) {
    # at first, the cache for the inverse is not set
    inverseCache <- NULL
    set <- function(y) {
        x <<- y
        #reset the inverse cache.
        inverseCache <<- NULL
    }
    get <- function() x
    
    #private functions
    setInverse <- function(newInverse) inverseCache <<- newInverse
    getInverse <- function() inverseCache
    
    #The returned object: with getter and setter functions.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Return a matrix that is the inverse of the content of 'x'
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    
    #If we have the cached inverse.
    if(!is.null(m)) {
        #message("getting cached data")
        return(m)
    }
    
    #Else, compute the inverse...
    data <- x$get()
    m <- solve(data, ...)
    #... And put it in the cache
    x$setInverse(m)
    
    #return the inverse
    m
}

test_cache <- function()
{
    testDim = 100
    rawMat <- matrix(rnorm(testDim*testDim, 50, 30), testDim, testDim)
    m1 = makeCacheMatrix(rawMat)
    expect_equal(rawMat, m1$get(), info="Test the get func.")
    expect_equal( m1$get() %*% cacheSolve(m1), diag(1, testDim), info="test the first pass, without cache")
    expect_equal( m1$get() %*% cacheSolve(m1), diag(1, testDim), info="test the second pass, with cache")
    
    testTime = 1000
    rawMatTime <- matrix(rnorm(testTime*testTime, 50, 30), testTime, testTime)
    m2 = makeCacheMatrix(rawMatTime)
    t1 <- system.time(inv1 <- cacheSolve(m2))[1];
    t2 <- system.time(inv2 <- cacheSolve(m2))[1];
    expect_more_than(t1, 10*t2, info="Time for the first call should be a lot more than time for the cache fetch.")
    
    
}
