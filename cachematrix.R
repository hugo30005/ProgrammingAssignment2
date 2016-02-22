-## Put comments here that give an overall description of what your
 -## functions do
  
  ## Write a short comment describing this function
  
  makeCacheMatrix <- function(x = matrix()) {
 -
 + #I start by creating an variable with only NULL values
 + inv <- NULL 
 + #I then create a function that will make z (a var from another environment) remember what x is
 + set<- function(z) {
 +   x<<- z
 +   #I also make inv be NULL in the other environment
 +   inv <<- NULL
 + }
 + #I create a function that will give me my matrix x
 + get<- function() x
 + #I create a function that will make inv be in the other environment
 + setinverse<- function(inverse) inv<<- inverse
 + #I create a function that will give me the inverse of x
 + getinverse <- function() inv
 + #Keep these four functions on a list
 + list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }
  
  
 -## Write a short comment describing this function
 -
  cacheSolve <- function(x, ...) {
 -        ## Return a matrix that is the inverse of 'x'
 +  #I make inv equal to the inverse of the funciton x
 +  inv<- x$getinverse()
 +  #If the inverse of function x has already been stored in the cache, this will get it from it
 +  if(!is.null(inv)) {
 +    message("getting cached data")
 +    return(inv)
 +  }
 +  #If inverse of has not yet been stored, we have to get it.
 +  #I create data, which will store the values of my matrix x
 +  data<- x$get()
 +  #I create inv, which will store the values of inverse of x
 +  inv<- solve(data, ...)
 +  #I call the function setinverse so I can store the values of inv in the cache
 +  x$setinverse(inv)
 +  #I print inv
 +  inv
  }
