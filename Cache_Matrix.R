makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # Limpia el valor de cache
    setmatrix <- function(y) {
        x <<- y # Cambia la matriz x por y
        m <<- NULL
    }
    getfunction <- function() x
    setsolve <- function(solve) m <<- solve # Invierte la matriz
    getsolve <- function() m # Si no existe la matriz, devuelve NULL, si existe 
                             # devuelve la inversa
    # Crea la lista con las 4 funciones
    list(ConfigMatrix = setmatrix, FuncionMatrix = getfunction,
         setsolve = setsolve,
         getsolve = getsolve)
}

# Funcion que calcula la inversa
cacheSolve <- function(x, ...) {  
    m <- x$getsolve() #Cequea si existe la matriz
    if(!is.null(m)) { # si no existe devuelve el mensaje
        message("getting cached data")
        return(m)
    }
    data <- x$FuncionMatrix()
    m <- solve(data, ...) %*% data #Calcula la inversa de la matriz
    x$setsolve(m) #Guarda la inversa en cache
    m # retorna la inversa de la matriz
}
