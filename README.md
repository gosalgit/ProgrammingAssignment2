### This second programming assignment of Coursera - R Programming Course  32. 

The assignment is to write an R function that is able to cache potentially time-consuming computations.
For example, taking the mean of a numeric vector is typically a fast
operation. However, for a very long vector, it may take too long to
compute the mean, especially if it has to be computed repeatedly (e.g.
in a loop). If the contents of a vector are not changing, it may make
sense to cache the value of the mean so that when we need it again, it
can be looked up in the cache rather than recomputed. 

In this Programming Assignment the advantage is taken of the scoping rules of
the R language and how they can be manipulated to preserve state inside
of an R object.

## Coursera-R-Programming Assignment 2
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## For this assignment, it is assumed that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) 
{
  inverseMat <- NULL
  set <- function(y) {
    x <<- y
    inverseMat <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inverseMat <<- inverse
  getInverse <- function() inverseMat
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## For this assignment, it is assumed that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) 
{
  inverseMat <- x$getInverse()
  
  if (!is.null(inverseMat)) {
    message("Retrieving the inverse from the cache")
    return(inverseMat)
  }
  
  matData <- x$get()
  inverseMat <- solve(matData, ...)
  x$setInverse(inverseMat)
  inverseMat
}