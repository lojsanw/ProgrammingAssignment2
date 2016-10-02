# File created on 28 Sept 2016 to create code for the Week 3 assignment
# for RProgramming

# These functions create the inverse of a matrix and store it, enabling it
# to be retrieved when needed.

# This function, makeCacheMatrix, creates an R object that stores a matrix
# and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # sets initial value of the inverse in the makeCM env
    set <- function(y) { 
      x <<- y  # use of the <<- operator assigns the value of y to x in 
               # the parent environment to the function
      inv <<- NULL # likewise, this assigns value of null to inv in
                   # the parent environment
    }
    get <- function() x # enables user to extract current value of x
                        # from the parent environment
    setinv <- function(inverse) inv <<- inverse # assigns setinv
                        # to the value of inverse in the parent env. This
                        # will be null if cacheSolve hasn't been run, and
                        # the inverse of the matrix x if cacheSolve has been
                        # run. 
    getinv <- function() inv # enables user to return the current value of 
                             # inv from the parent environment 
    list(set = set, get = get, # gives names to each element of list created
         setinv = setinv,      # by the function
         getinv = getinv)
}

myMatrix <- makeCacheMatrix(matrix(2:5,2,2))
myMatrix
myMatrix$get()
myMatrix$getinv()


# This function, cacheSolve, retrieves the matrix inverse stored in the 
# makeCacheMatrix environment. If the inverse does not exist, the function
# creates it. It is then stored in the mCM function environment.

cacheSolve <- function(x, ...) {# Returns a matrix that is the inverse of 'x'
  inv <- x$getinv() # assigns inv the value of the inverse stored in the 
                    # myMatrix environment -- value will either be null or 
                    # the inverse previously instantiated.
  if(!is.null(inv)) { # checks whether inv is null or exists
    message("getting cached data") 
    return(inv)  # returns inv if it exists with the message "getting cached data"
  }
  data <- x$get() # if inv is null, gets input data x from myMatrix
  inv <- solve(data, ...) # creates inverse from that data
  x$setinv(inv) # sets inverse in function x to inv
  inv # returns inv
}  

cacheSolve(myMatrix)

myMatrix$get()
myMatrix$getinv()

rm(cacheSolve, myMatrix, makeCacheMatrix)

