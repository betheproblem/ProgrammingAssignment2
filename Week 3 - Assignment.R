#ExampleGiven
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

#ExampleGiven2
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


#Function below creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
  
#Setting the value of the matrix
  i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
#Getting value of the matrix
    get <- function()x
    
#Set value of the inverse
    setinverse <- function(inverse) i <<- inverse
    
#Get value of the inverse
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}



#Computes the inverse of a special matrix returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  
#Get cached data and see if the inverse has already been calculated
    i <- x$getinverse()
    if(!is.null(i)) {    #Checks if not NULL
          message("getting cached data")
          return(i)
    }
    
#Retrieve the inverse from the cache
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}





