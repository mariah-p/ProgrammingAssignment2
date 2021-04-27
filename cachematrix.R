## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix() takes a matrix as an argument and returns an object in the
# form of a named list that contains a function in each of 4 list items.
#
#   *1 the set() function takes the object x that is input as an argument and sets
#      the value of m to NULL whenever a new x is set
#   *2 the get() function uses lexical scoping to retrieve the value of x from the
#      parent environment (e.g. whatever is set to x in the argument)
#   *3 the setsolve() function sets the inverse of the provided matrix and assigns
#      it to m from the parent environment
#   *4 the getsolve() uses lexical scoping to find the value for m outside of the 
#      getsolve() function

# cacheSolve() populates or retrieves the inverse of the given matrix from an
# object of type makeCacheMatrix()
#
#   *1 it attempts to retrieve a cached inverse stored in the $getsolve list item
#   *2 if the inverse is not NULL then it returns the cached inverse to the parent 
#      environment
#   *3 if $getsolve is NULL then it takes the matrix that was input as an argument
#      calculates the inverse and sets the inverse into the cache using setsolve()
#      then returns the inverse to the parent environment


## Write a short comment describing this function

# creates an object of type makeCacheMatrix() that contains a list of 4 functions
# that set and get the values for a matrix input as an argument and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)        
}


## Write a short comment describing this function

# cacheSolve() either retrieves the cached inverse for the matrix that is input 
# as an argument or sets and calculates a new inverse if it has not been cached

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}