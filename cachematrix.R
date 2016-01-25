## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix function makes a special list of function that do the following
##1.set the value of a matrix input
##2.get the matrix
##3.set the value of the matrix's inverse
##4.get the matrix's inverse
##the values are stored as a list in the end of the function

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL 
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function()x
      setinverse<-function(inverse) m<<-inverse
      getinverse<-function() m
      list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
##cacheSolve function calls the functions in makeCacheMatrix and
##calculate the inverse of the matrix if the inverse is not already in the cache
##and return the message "getting cached data" and inverse of the matrix if the inverse is alreay in the cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

