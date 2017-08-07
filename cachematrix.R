## This function is first setting the value of a matrix, then creates variables
## to test if the matrix has already been cached. 

makeCacheMatrix <- function(x = matrix()) {
        ## Set value of matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Get value of matrix
        get <- function() x
        ## Set value of mean
        setmatrix <- function(matrix) m <<- matrix
        ## Get the value of the mean
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## The cacheSolve function returns a matrix that is an inverse of x by taking 
## previously defined variable x and using the solve function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
