## 
## Assignment 2 - Functions to Cache the Inverse of a Matrix
##
## Numerical operations such as Matrix Inversion are computationally
## expensive, such that it is often more efficient to cache the result of a
## computation in a parent environment rather than recompute it.  In this 
## assignment we define two functions, one to set and get the values of the
## inverse of a matrix, and another to calculate and cache the inverse of
## the matrix.
##
## History:
##   2014-09-21   Daniel J. Valentino   created for Assignment 2
##
## References:
##   Coursera Data Science Specialization: R Programming (www.coursera.com)



## makeCacheMatrix() - function to set and get the values of a matrix object
makeCacheMatrix <- function(the_mat = matrix()) {

  # the <<- operator is used to set the environment to the parent environment of
  # the current environment
  
  # initialize variable inv_mat (inverse matrix) to NULL
  inv_mat <- NULL
  
  # set_mat() -- function to set the matrix object (the_mat) in the parent 
  # environment to the value of the argument matrix (arg_mat), and to 
  # set the variable inv_mat in the parent environment to NULL
  set_mat <- function(arg_mat) {
    the_mat <<- arg_mat
    inv_mat <<- NULL
  }
  
  # get_mat() -- function to return the value of the matrix object (the_mat));
  # it first looks for the_mat in the current environment (the environment of 
  # the get_mat() function) and then in the parent environment (the environment  
  # of the makeCacheMatrix() function).
  get_mat <- function() the_mat
  
  # set_inv() -- function to set the value of the copy of inv_mat in the 
  # *parent* environment to the argument passed to the function.
  set_inv <- function(arg_mat) inv_mat <<- arg_mat
  
  # get_inv() -- function to return the value of inv_mat (the inverse matrix); 
  # the function first looks for m in the current environment, then in the 
  # parent environment.
  get_inv <- function() inv_mat
  
  # creates a labeled vector of the functions set_mat, get_mat, set_inv and 
  # get_inv; makeCacheMatrix returns the list.
  list(set_mat = set_mat, get_mat = get_mat,
       set_inv = set_inv, get_inv = get_inv)  
}


## cacheSolve() -- function to compute the inverse of a "matrix" object; if the
## inverse has already ben calculated and the matrix object has not been 
## changed, then the cacheSolve() function should retrieve the inverse of the
## matrix from the cache.

cacheSolve <- function(x, ...) {
  # check if the inverse has been computed
  cmat <- x$get_inv()
  
  # if yes, return the inverse from cache
  if( !is.null(cmat)) {
    message("getting inverse from cache")
    return( cmat )
  } else {
    # if not, get the matrix from the object, then calculate the inverse of 
    # the matrix using solve() function, and then set and return the inverse()
    message("calculating inverse")
    the_mat <- x$get_mat()
  
    inv_mat = solve(the_mat)
  
    x$set_inv( inv_mat )
    return( inv_mat )
  }
}

## unit tests

# generate an invertible matrix
sq_mat = rbind( c(1,-1/4), c(-1/4, 1))

# create the matrix object
my_mat = makeCacheMatrix( sq_mat )

print( sq_mat )
print( my_mat )

# product should be the Identity Matrix
print (inv_mat %*% sq_mat)

# create and cache the Inverse (first time)
result1 = cacheSolve(my_mat)
print( result1 %*% sq_mat)

# retrieve Inverse from Cache (second time)
result2 = cacheSolve(my_mat)
print( result2 %*% sq_mat)