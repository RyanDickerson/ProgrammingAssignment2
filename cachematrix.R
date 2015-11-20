#set: sets the value of the matrix stored in the main function
#get: returns the value of the matrix stored in the main function
#setinverse: stores value of input in a variable inv
#getinverse: returns inv

makeMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#if portion checks if inverse has been computed already and returns it if so
#stuff below if portion computes inverse of new matrix and returns it 

cacheinverse <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
