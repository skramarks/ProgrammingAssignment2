#
# This "class" like collection of functions are used efficiently calcuate the inverse of
# a matrix by calculating it once and storing the result for future use.
#
# The function makeCacheMatrix creates the object.
# The function cacheSolve calculates the inverse of the matrix if nexessary and returns it.



makeCacheMatrix <- function(originalMatrix = matrix()) {
    # 
    # Creates a "CacheMatrix" object that is used to  store the original matrix and a 
    # transformed version of it.
    #
    # Parameter:
    #
    #   originalMatrix  The matrix to be stored for transform
    #
    #
    #   Usage example:
    #       mm <-matrix(c(1, 0, 1, 2,  4,0, 3, 5, 6), 3, 3)
    #       mmm <- makeCacheMatrix(mm)
    #       cacheSolve(mmm)
    
    
    
    # initially no transformed matrix
    transformedMatrix <- NULL

    # Store a matrix.
    setOriginal <- function(original) {
        
        # store these values in the parent context (that of the object).
        originalMatrix <<- original
        
        # erase an old transformed matrix
        transformedMatrix <<- NULL
    }
    
    
    # fetch the stored matrix
    getOriginal <- function() originalMatrix
    
    
    #
    # Stores the transformed matrix in the object.
    #
    setTransformed <- function(transformed) transformedMatrix <<- transformed
    
    #
    # Returns the stored transformed matrix
    #
    # The return value will be NULL if it has not been set
    #
    getTransformed <- function() transformedMatrix
    
    
    #
    # create and return the "object"
    #
    list(setOriginal = setOriginal, getOriginal = getOriginal,
         setTransformed = setTransformed,
         getTransformed = getTransformed)
}


#
#
cacheSolve <- function(x, ...) {
    # 
    # Returns the inverse of a matrix.   If the inverse was not previosly 
    # calculated, it is calculated and stored in the object for reuse.
    #
    # Parameter:
    #
    #   x   an object created by makeCacheMatrix
    #
    #   Returns:  the inverse of the original matrix
    #
    
    invertedMatrix <- x$getTransformed()
    
    if(!is.null(invertedMatrix)) {
        #
        # The inverse was already calcualted, return it.
        #
        message("getting cached data")
        return(invertedMatrix)
    }
    
    # Calculate the inverse
    data <- x$getOriginal()
    invertedMatrix <- solve(data, ...)
    
    # cache the result
    x$setTransformed(invertedMatrix)
    
    # return a copy of the inverted matrix
    invertedMatrix
}
