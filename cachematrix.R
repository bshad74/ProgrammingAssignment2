## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "vector", which is 
## really a list containing a function to

## set Matrix
## get Matrix
## set Inverse
## get Inverse

##=============================================================================



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    mINV <- NULL
    set <- function(y) {

        ## <<- Assignment Operator so that the object value can
        ## be assigned to environment different from current
        
        x <<- y 
        mINV <<- NULL  
                        
    }
    get <- function() x
    setinv <- function(inverse) mINV <<- inverse
    getinv <- function() mINV
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

## inverse gets Computed using the solve function. 

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    mINV <- x$getinv()
    
    ## If statement checkes to see If the inverse has already been 
    ## calculated
    
    if(!is.null(mINV)) {
        
        ## If yes, than print the following message and get it from Cache     
            
        message("getting cached data")
        return(mINV)
    }
    
    ## If Not, than calculate the inverse 
    
    data <- x$get()
    mINV <- solve(data, ...)
    
    ## Once the inverse has been calculated, set the value into cache
    
    x$setinv(mINV)
   
    return(mINV)
}

