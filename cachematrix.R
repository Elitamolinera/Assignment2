#This function caching the inverse of a matrix
#Consider the restrictions to calcule a matrix inverse

## Esta función crea una matriz especial de objeto que puede almacenar
##  en cache su inversa, en esta función no se visualiza dicho resultado.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvmat <- function(invmat) inv <<- invmat
  getinvmat <- function() inv
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}

## Esta función coge la matriz especial de objeto almacenada en cache  
## de la función makeCacheMatrix y muestra el resultado (inversa matriz)

cacheSolve <- function(x, ...) {
  invmat <- x$getinvmat()
  if(!is.null(invmat)) {
    message("getting cached matrix")
    return(inv)
  }
  matrixx <- x$get()
  inv <- solve(matrixx, ...)
  x$setinvmat(inv)
  inv
}

