##makeCacheMatrix: This function creates a special "matrix" object
##that can cache its inverse.

###First step: initialization of x and inv objects: 
###x as an argument and inv is set to NULL. 
makeCacheMatrix <- function(x = matrix()) {
###initializing inv as an object within the makeCaheMatrix() environment to be 
###used by later code in the function.
  inv <- NULL 
###Second: defining the set() function.
  set <- function(y) { 
###we use the <<- form of the assignment operator, 
###which assigns the value on the right side of the operator to an object 
###in the parent environment named by the object on the left side of the operator.
    x <<- y 
    inv <<- NULL
  }
###Third, makeCaheMatrix() defines the getter for the matrix x
###Since the symbol x is not defined within get(), 
###R retrieves it from the parent environment of makeCaheMatrix()
  get <- function() x 
###Fourth, defines the setter for the inverse inv.
### we have to use the <<- form of the assignment operator 
###to assign the input argument to the value of inv in the parent environment.
  setInverse <- function(Inverse) inv <<- Inverse
###Finally, defines the getter for the inverse inv
  getInverse <- function() inv
###assigning each of these functions as an element within a list(), 
###and returns it to the parent environment.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

###First step: CacheSolve starts with a single argument, x, and an ellipsis 
###that allows the caller to pass additional arguments into the function.
cacheSolve <- function(x, ...) {
###the function attempts to retrieve a inverse from the object passed in as the argument
  inv <- x$getInverse()
###it checks to see whether the result is NULL
###if the value here is not equal to NULL, we have a valid, 
###cached inverse and can return it to the parent environment
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
###If the result of !is.null(m) is FALSE, cacheSolve() gets the matrix from 
###the input object, calculates a inverse(), uses the setInverse() function 
###on the input object and then returns the value of the inverse to the parent environment
  data <- x$get()
  inv <- Inverse(data, ...)
  x$setInverse(inv)
  inv     ## Return a matrix that is the inverse of 'x'
}
