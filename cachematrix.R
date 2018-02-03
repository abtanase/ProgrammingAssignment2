## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# makeVectorCorrected <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function() m <<- mean(x)
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

# set a vector
v2 <- makeVector()
v2$get()
v2$set(c(1,2,3))
v2$get()
v2$getmean() # NULL! 
v2$setmean(mean(v2$get()))
v2$get()
v2$getmean()
v2$setmean(15)
v2$getmean()
v2$get() # and now the data is misaligned with the mean...

# set a vector
v2 <- makeVectorCorrected()
v2$get()
v2$set(c(1,2,3))
v2$get()
v2$getmean() # NULL! 
v2$setmean()
v2$getmean() correct 
v2$setmean(5) # error, one cannot set the mean, only prompt for its computation
v2$get()
v2$getmean()
v2$setmean(15)
v2$getmean()
v2$get() # and now the data is misaligned with the mean...

cachemean(v2)

# Matrix exercise
# makeCacheMatrix
makeCacheMatrix <- function(x=matrix()){
  m <- NULL
  set <- function(y){
    # only reset the inverse if a new matrix is indicated
    if (any(dim(x)!=dim(y))){
      x <<- y
      m <<- NULL # don't compute inverse until asked to
    } else {
      if(!identical(x,y)){
        x <<- y
        m <<- NULL # don't compute inverse until asked to
      }
    }
  }
  get <- function() x
  setInverse <- function() m <<- solve(x) # note the difference - the setInverse is the command to compute the inverse in the cacheMatrix object
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, newmatrix=NULL, ...){
  # first compare stored data with the new matrix (if given)
  if(!is.null(newmatrix)){
    if(!identical(x$get(),newmatrix)){
      x$set(newmatrix)
    }
  }
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  x$setInverse()
  m <- x$getInverse()
  m
}

myMat <- makeCacheMatrix() # "empty" container
A <- matrix(c(1,2,3,4), ncol=2) # matrix
myMat$set(A)
myMat$get()
myMat$getInverse()

cacheSolve(x=myMat)
B = matrix(c(1,3,5,7), ncol=2)
cacheSolve(x=myMat, newmatrix = B)