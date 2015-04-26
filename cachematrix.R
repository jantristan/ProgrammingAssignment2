## Cache the inverse of Matrix
## 

## makeCMatrix creates a special vetor, which is a list containing a fx to
##      1.) set the value of the vector
##      2.) get the value of the vecor
##      3.) set the value of the mean
##      4.) get the value of the mean

makeCMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y){
    x <<- y
    inv <<- NULL
  }

  get=function()x
  setinv=function(inverse) inv<<-inverse
  getinv=function()inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve returns the inverse of the matrix in makeCMatrix

cacheSolve <- function(x, ...) {
  inv=x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
