## The function makeCacheMatrix creates a list of 4 functions to

## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the solved/inversed matrix (setsolve)
## 4. get the solved/inversed matrix (getsolve)


makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function () {
    x
  }
  setsolve <- function(solve) {
    s <<- solve
  }
  getsolve <- function() {
    s
  }
  list(set = set, 
       get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}


## The function cacheSolve calculates the inverse matrix (solve())
## of the matrix created in the makeCacheMatrix function. Before it
## calculates the inverse matrix, it checks if it has been 
## calculated before and if so skips the rest of the function and
## returns the result (s). If the inverse matrix has not been 
## calculated, cacheSolve does so and caches the result (setsolve)

cacheSolve <- function(x, ...) {
                        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()     ## get the matrix from makeCacheMatric
  if(!is.null(s)){      ## check if the inverse matrix has been calculated
    message("getting cached data")
    return(s)           ## return the cached inverse matrix
  }
  data <- x$get()       ## else, get the data
  s <- solve(data, ...) ## calculate the inverse matrix
  x$setsolve(s)         ## cach the inverse matrix via setsolve 
  s                     ## return the inverse matrix
}
