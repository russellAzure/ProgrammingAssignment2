## Create a special matrix that allows caching of its inverse
## in order to avoid too many re-calculations of the inverse

## create a speacial matrix data type, which is essentially a list that allows setting the value of the matrix and retrieving it later, as well as
## setting the value of its inverse and retrieving it later

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function (y) {
x <<- y
i <<- NULL
}
get <- function() x
setInverse <-function(inverse) i <<- inverse
getInverse <-function() i
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Reads input matrix, then checks if inverse of matrix is already cached in the special matrix data type. If it is, return cached value. If not, compute
## the inverse with solve(), save it to special matrix, and output it

cacheSolve <- function(x, ...) {
i <- x$getInverse()
if(!is.null(i)){
message("Getting cached data")
return (i)
}
data <- x$get()
i <- solve(data) %*% data
x$setInverse(i)   
i
## Return a matrix that is the inverse of 'x'
}


