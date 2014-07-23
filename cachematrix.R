## Functions created to cache and call the inverse of a matrix

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
  {
    m<-NULL
    set<-function(y)
      {
        x<<-y
        m<<- NULL
      }  
    get<-function()x
    setinverse<-function(solve)m<<-solve
    getinverse<-function()m
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
  }

## This function computes the inverse of the special matrix returned by "makeCacheMatrix".
## If the inverse has already been calculated, then this function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
  {
        m<-x$getinverse()
        if(!is.null(m))
        {
          message("getting cached data")
          return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
  }

tee = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
tee$get()         # Matrix
tee$getinverse()      # Matrix inverse
cacheSolve(tee)   # Cached matrix inverse using previously computed matrix inverse

tee$get()%*%tee$getinverse() ##Checking inverse
