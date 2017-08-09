makeCacheMatrix <- function(x = nuemeric()) {
    m <- NULL
    setmatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getfunction <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(ConfigMatrix = setmatrix, FuncionMatrix = getfunction,
         setsolve = setsolve,
         getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$FuncionMatrix()
    m <- solve(data, ...) %*% data
    x$setsolve(m)
    m
}
