


 makeCacheMatrix <- function(x = matrix()) {

      xinversion <- NULL #  result of inversion is stored
      # A setter function, use this to set a matrix to object created by makeCacheMatrix function
      set <- function(y) {
	  x <<- y
	  xinversion <<- NULL # it also initialises xinversion to null
      }

      get <- function() x # return the input matrix
      setInv <- function(inv) xinversion <<- inv # set the inversed matrix
      getInv <- function() xinversion # return the inversed matrix
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)
  }

  cacheSolve <- function(x, ...) {
      m <- x$getInv() # get the inversed matrix from object x
      # it will be null if uncalculated, remember the first line "xinversion <- NULL" in the previous function
      if(!is.null(m)) { # if the inversion result is there
	  message("getting cached data")
	  return(m) # return the calculated inversion
      }
      data <- x$get() # if not, we do x$get to get the matrix object
      m <- solve(data) # we solve it
      x$setInv(m) # we then set it to the object
      m # return the solved result
  }

# Test
  # generate a random square, non-singular matrix
  test <- matrix(runif(9,1,100),3,3)
  # generate the makeCacheMatrix object with this matrix
  testCached <- makeCacheMatrix(test)
  # from now on calculate or retrieve calculated inversion using the cacheSolve function

  testInv <- cacheSolve(testCached)
  testInv <- cacheSolve(testCached)
  testInv <- cacheSolve(testCached)
  testInv <- cacheSolve(testCached)
  testInv <- cacheSolve(testCached)