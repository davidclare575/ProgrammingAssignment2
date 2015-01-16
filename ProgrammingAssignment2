###there are two functions in here that in the end are supposed to take a
###matrix and store it as a cached version that has the inverse already
###solved.  The idea is that inverting matrices can be a little time and
###computationally expensive and so instead of repeatedly finding the inverse
###of a matrix it can do it once and whenever you want that inverse it just
###calls up a already inverted matrix.
###The first function "makeCacheMatrix" puts the matrix you have into cache.
###The way you would use it is assign to a new object the function with the
###matrix you want cached as the argument.
###The second function "cacheSolve" will give the inverse of that cached
###object when you pass as the argument the object that had makeCacheMatrix
###assigned to it.  It first checks to see if the inverse for that object has
###already been made.  If it has, it just pops it out.  If it hasn't, it
###inverts the matrix on the spot and saves it for the future.

###This "makeCacheMatrix" function will store the matrix to cache under the
###you assign the function to.  Pass the matrix you want stored as x.
###I've found it's best to name the cached one different than the one you are
###putting in.
makeCacheMatrix <- function(x = matrix()) {
        ##this m variable is the inversion.  Setting to empty at first.
	  m <- NULL
        ##this part is what caches the matrix.
	  set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##this is the part that would print the matrix out
	  get <- function() x
        ##this would be the part that is called to invert.
	  setinverse <- function(solve) m <<- solve
        ##this part is how that inversion would be called up.
	  getinverse <- function() m
        ##this is all of that rolled into a return.
	  list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

###This function "cacheSolve" is to solve the matrix if it hasn't already been
###done.  It will return the inversion.  You pass the object that you assigned
###the previous function to as the argument.
cacheSolve <- function(x, ...) {
        ###This part goes and looks for the inversion.
	  m <- x$getinverse()
        ###This part checks to see if that inversion has been done.
	  ###If it has been done, it tells you so and returns that.
	  if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ###Otherwise, it goes and does the inversion, saves it, and returns it.
	  data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
