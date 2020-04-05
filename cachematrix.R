## Put comments here that give an overall description of what your
## functions do

# Function "makeCacheMatrix" takes the matrix whose inverse has to be 
# calculated and return a list of functions; which are then passed
# to "cacheSolve" function. If the inverse has already been calculated
# then it's returned from the cache value else it is calculated in the 
# second function.



## Write a short comment describing this function

# This function takes a matrix "x" as it's argument
# and return a list of functions given as -
#  1)set_matrix -> 
#  2)get_matrix ->
#  3)set_inverse ->
#  4)get_inverse ->

makeCacheMatrix <- function(x = matrix()) {
  inver<-NULL
  set_matrix<-function(y){
    x<<-y
    inver<<-NULL
  }
  get_matrix<-function(){
    x
  }
  set_inverse<-function(inverse){
    inver<<-inverse
  }
  get_inverse<-function(){
    inver
  }
  list(set_matrix=set_matrix,get_matrix=get_matrix,set_inverse=set_inverse,get_inverse=get_inverse)
}



## Write a short comment describing this function

# This function takes the list of functions created by makeCacheMatrix
# as it's argument and compute the inverse of the matrix if 
# the inverse doesn't already exist in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse<-x$get_inverse()
  if(!is.null(inverse)){
    message("Getting Cached Data")
    return(inverse)
  }
  data<-x$get_matrix()
  data
  inverse<-solve(data,...)
  x$set_inverse(inverse)
  inverse
}
