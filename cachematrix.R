## then caches it so that the user is able to save resources
## by using previously saved value when repeating calculations
## to get the inverse

## Creates a matrix object containg a function to get and then set value of inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
   }
  get<-function() x
  setinverse<-function(solve) m<<- solve
  getinverse<-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

#Calculates the inverse of the matrix created with the above function

cachesolve <- function(x=matrix(), ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinverse(m)
  m
}