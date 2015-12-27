## These functions return the inverse of a square "invertable" matrix. 
## If the matrix is already cached, we return the cached inverse.
##
##
## makeCacheMatrix - This function creates a special matrix that can cache it's inverse. It
## accepts a matrix, and can be used to set/get the inverted matrix.
makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        ## initializes the function 
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        ## returns the initialized matrix
        get <- function() x
        ## set the inverted matrix
        setinver <- function(inverse) inver <<- inverse
        ## returns the inverted matrix
        getinver <- function() inver
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)
}

## cacheSolve - Evaluates if the matrix is already cached, then returns the computed 
## inverse of the matrix returned by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inver <- x$getinver()
        
        if(!is.null(inver)){
                ## Return cached matrix
                message("Retrieving cached data")
                return(inver)
        }
        ## Get matrix and invert it
        data <- x$get()
        inver = solve(data, ...)
        
        x$setinver(inver)
        ## Return  inverted matrix
        inver
}
