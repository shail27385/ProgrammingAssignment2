## Function - Cache the inverse of Matrix
# makeCacheMatrix 
# list of function
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of inverse of the matrix
# 4. Get the value of inverse of the matrix

makeCacheMatrix <- function(mydata = matrix()) 
{
  inv <- NULL
  setdata <- function(y) 
  {
    mydata <<- y
    inv <<- NULL
  }
  getdata <- function() mydata
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(setdata=setdata, getdata=getdata, setinverse=setinverse, getinverse=getinverse)
}

# function returning the inverse of the matrix.
# If the inverse has already been computed, get the result and skips the computation.
# If not, it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(mydata) 
{
  inv <- mydata$getinverse()
  if(!is.null(inv)) 
  {
    message("getting cached data.")
    return(inv)
  }
  data <- mydata$getdata()
  inv <- solve(data)
  mydata$setinverse(inv)
  inv
}

######################## Dry run  ###########################

## x = rbind(c(1,4,1,1), c(1,4,0,1), c(2,3,1,2), c(3,2,6,4))
## m = makeCacheMatrix(x)

## Get the Data of the Matrix
## m$getdata()

##    [,1] [,2] [,3] [,4]
##[1,]    1    4    1    1
##[2,]    1    4    0    1
##[3,]    2    3    1    2
##[4,]    3    2    6    4

## No cache in the first run
## cacheSolve(m)
##    [,1] [,2] [,3] [,4]
##[1,]  3.2 -4.8  2.8   -1
##[2,]  0.2  0.2 -0.2    0
##[3,]  1.0 -1.0  0.0    0
##[4,] -4.0  5.0 -2.0    1

## Retrieving from the cache in the second run
## cacheSolve(m)

##getting cached data.
##    [,1] [,2] [,3] [,4]
##[1,]  3.2 -4.8  2.8   -1
##[2,]  0.2  0.2 -0.2    0
##[3,]  1.0 -1.0  0.0    0
##[4,] -4.0  5.0 -2.0    1
