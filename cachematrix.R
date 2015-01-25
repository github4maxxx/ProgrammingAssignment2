##The program has been written to calculate the inverse of a matrix. Given that
##calculating inverse is a time-taking calculation, the code below enables
##caching the inverse of a matrix along with the matrix itself.


##The function below stores a matrix and its inverse. Whenever a new
##matrix is assigned to the function, the inverse is set to null and
##has to be calculated and cached using the function "cacheSolve"
makeCacheMatrix <- function(x=matrix()) {
  x_inv <- NULL
  
  #set the matrix value
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  
  #get the matrix value
  get <- function() x
  
  #set the matrix inverse
  setinv <- function(y_inv) x_inv <<- y_inv
  
  #get the matrix inverse
  getinv <- function() x_inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}



##the function below enables calculation of inverse of a matrix object
##returned by the "makeCacheMatrix" above
cacheSolve <- function(x) {
  
  ##getting the previously cached matrix and inverse value
  cached_inv <- x$getinv()

  if (!is.null(cached_inv)) {
    ##checking if the inverse has already been calculated

    message("Skipping re-calculation since the inverse is not null")
    return(cached_inv)
    
  } else {
    ##Inverse is missing, hence recalculating the same and storing it in cache
    
    cached_inv <- solve(x$get())
    x$setinv(cached_inv)
    cached_inv
    
  }
  
}
