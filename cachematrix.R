## makeCacheMatrix function and cacheSolve functions are used to obtain inverse matrices


## makeCacheMatrix function contains a list of functions to set the value of the matrix,
## get the value of the matrix, set value of inverse, get value of inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # initialise the inverse matrix 
  inv <- NULL
  
  # set the value of the matrix
  
  set <- function(y){
    x <<-y
    inv <<- NULL
  }

  # obtain the value of the matrix
  
  get <-function() x
  
  #set the value of the inverse
  
  set_inverse<-function(inv_input) inv <<- inv_input
  
  #get the value of the inverse
  
  get_inverse <- function() inv
  
  #return a list of the functions
  
  list(set=set,get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## cacheSolve function checks to see if the inverse has already been calculated
## if not it calcualtes the inverse of the matrix 

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  # ohterwise, we get the matrix
  data <- x$get()
  # alculate the inverse
  inv <- solve(data, ...)
  # cache the inverse of the matrix
  x$set_inverse(inv)
  # return the result
  inv
}
