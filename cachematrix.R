## These functions use R’s lexical scoping to cache the inverse of a matrix after it has been calculated once.  makeCacheMatrix caches the original matrix and inverse, setting inverse to NULL.  cachesSolve function returns inverse of matrix.  If it’s been calculated before it just returns the cache value.  If it hasn’t been calculated before, i.e. value of NULL, it gets the matrix and using solve function to find inverse and caches this inverse.

## makeCacheMatrix is passed a matrix and creates an object of type list.  It stores the original matrix and the inverse of the matrix, setting inverse to NULL.  There are 4 methods, set, get, setinverse and getinverse.  The method set sets the matrix value, get returns the matrix, setinverse calculates the inverse and returns it's value.

## The set method uses supper-assignment <<- to set the matrix and inverse to containing environment.  The setinverse method also uses <<- to set inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                    # inverse set to NULL
    # set method to super-assign matrix and NULL value for inv.
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    get <- function() x                             # returns matrix
    setinverse <- function(solve) inv <<- solve     # sets inverse
    getinverse <- function() inv                    # returns inverse
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
    
}

# cacheSolve is passed the object created by makeCacheMatrix.  It gets the value cached.  If not NULL, it simply returns the cache value and exits.  If inverse value is NULL, then the get method is used to get the matrix and inverse is found using solve function. This calculated inverse is then set and cached and value returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()                            # gets cached inverse matrix
    # if cache value of inverse is not NULL, gets this value and returns it
    if(!is.null(inv)) {                              
        message("getting cache data")
        return(inv)                                  # return exits funcions                             
    }
    data <- x$get()                                  # gets cache matrix
    inv <- solve(data, ...)                          # calculates inverse matrix
    x$setinverse(inv)                                # caches this inverse
    inv                                              # returns inverse matrix
}
