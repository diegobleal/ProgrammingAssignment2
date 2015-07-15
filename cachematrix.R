# The following two functions calculate the inverse of a matrix 
#and save it to the cache. 
# 
# The first function creates a special "matrix" object, or a list 
#containing a function to: 
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# Set the value of the inverse
# Get the value of the inverse

makeCacheMatrix<-function(x=matrix()){
     m<-NULL
     set<-function(y){
          x<<-y
          m<<-NULL
     }
     get<-function() x
     setinverse<-function(inverse) m<<-inverse
     getinverse<-function() m
     list(set=set, get=get, setinverse=setinverse,
          getinverse=getinverse)
}

#The second function calculates the inverse of the special matrix 
#created above. 
#It first checks to see if the inverse has already been calculated. 
#If the calculated inverse exists, it gets the inverse from the cache. 
#Otherwise, it calculates the matrix inverse and sets the value of the 
#inverse in the cache via the setinverse function.

cachesolve<-function(x, ...){
     m<-x$getinverse()
     if(!is.null(m)){
          message('getting cached data')
          return(m)
     }
     data<-x$get()
     m<-solve(data,...)
     x$setinverse(m)
     m
}