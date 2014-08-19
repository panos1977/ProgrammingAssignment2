## This R script implements the caching the inverse of a matrix.
## For implementing of the caching the inverse of a matrix we use the following
## functions: makeCacheMatrix and cacheSolve. These two functions are used to
## create a special "matrix" object that stores a matrix and cache's its inverse.

## This function accepts as input an numeric matrix and returns a special "matrix"
## object which is really a list of four elements. Each element of this list 
## contains a function to set, get, setmatinv and getmatinv, respectively.
## The details of these functions are below. These functions are used by cacheSolve
## function as we will see bellow.

makeCacheMatrix <- function(x = matrix()) {

    # Set the matrix inverse, i.e. mat_inv to NULL every time
    # makeCacheMatrix is called
    mat_inv <- NULL
    
    # This function sets the value of the original matrix
    set <- function(y) {
        x <<- y
        mat_inv <<- NULL
    }
    
    # This function returns the value of the original matrix 
    get <- function() x
    
    # This function stores the value of the inverse of a matrix using the <<-
    # operator
    setmatinv <- function(inv) mat_inv <<- inv
    
    # This function returns the cached value of the inverse of a matrix
    getmatinv <- function() mat_inv
   
    # This list is returned with the newly created object. It contains
    # the four above functions that are part of the object
    list(set = set, get = get,
         setmatinv = setmatinv, getmatinv = getmatinv)
}


## This function accepts as input a special "matrix" object (i.e. 'x') returned by
## makeCacheMatrix above and returns a matrix that is the inverse of 'x'.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache, otherwise computes
## the inverse.

cacheSolve <- function(x, ...) {
        
    # Accesses the object 'x' and gets the value of the inverse    
    mat_inv <- x$getmatinv()
    
    # Examines if the inverse has already computed or cached
    if(!is.null(mat_inv)) {
        message("getting cached data") # Send this message to the console
        return(mat_inv)  # Returns the cached value of the matrix inverse
    }
    
    # The following lines of code computes the inverse of 'x' if x$getmatinv()
    # returned null
    
    data <- x$get() # Accesses the object 'x' and gets the value of the original matrix
    
    mat_inv <- solve(data, ...) # Calculates the inverse using the solve function in R
    
    x$setmatinv(mat_inv) # Stores the calculated value of inverse in x
    
    mat_inv # Returns the inverse to the code that called this function
}
