## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
# 1.  set the value of the vector
# 2.  get the value of the vector
# 3.  set the value of the mean
# 4.  get the value of the mean
# 


makeCacheMatrix <- function(x = matrix()) {
    #initializing the object required for the constructor
    invert <- NULL
    #creating the behaviours of the makeCacheMatrics object constructor
    set <- function(y){
        #We assign the "y" input argument tp the parent environment (that of "x")
        x <<- y
        #assign the NULL value to "invert" in the makeCacheMatrix environment
        #eliminating any previous value assigment in the cache for "invert"
        invert <<- NULL
    }
    #we now define the "getter function"
    get <- function() x
    #the setter of the inverted matrix
    setInvert <- function(solve) invert <<- solve
    #and finally the getter function for the inverted matrix
    getInvert <- function() invert
    #We now crethe the list that allows the constructur function to return a
    #fully formed object
    list(set = set, get = get, setInvert = setInvert, getInvert = getInvert)
    
}


## This function computes the inverse of the special "matrix" returned by 
##`makeCacheMatrix` above. If the inverse has already been calculated (and the 
##matrix has not changed), then`cacheSolve` should retrieve the inverse from 
##the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'cachemean <- function(x, ...) {
    invert <- x$getInvert()
    #We check wether there is already a cached object anreturn it, otherwise, we create it
    if(!is.null(invert)) {
        message("getting cached data")
        return(invert)
    }
    #WE call on the getter function of the constructor 
    data <- x$get()
    #calculat and assign the inverse matrix
    invert <- solve(data, ...)
    x$setInvert(invert)
    #and return it
    invert
}
