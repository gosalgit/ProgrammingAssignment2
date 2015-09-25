## Coursera-R Programming - 32 Assignment 2
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


## ======Sample Running of the Functions========

## > x = rbind(c(2, 7), c(7, 1))

## > m = makeCacheMatrix(x)

## > m$get()
##      [,1] [,2]
## [1,]    2    7
## [2,]    7    1

## First run without cache

## > cacheSolve(m)
##            [,1]        [,2]
## [1,] -0.0212766  0.14893617
## [2,]  0.1489362 -0.04255319

##  Subsequent run with cache data

## > cacheSolve(m)
## Retrieving the inverse from the cache 
##            [,1]        [,2]
## [1,] -0.0212766  0.14893617
## [2,]  0.1489362 -0.04255319