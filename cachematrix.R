##ProgrammingAssignment2
  #Q1
  ##Create a pair of function that can cache the inverse of a matrix called makeCachematrix
  ##The following function creates a special matrix object that can cache its inverse
  makeCachematrix <- function(x=matrix()) {
    inv<- NULL
    set<- function(y){
      x<<- y
      inv<<- NULL
      }
    get <- function() x
    setInverse <- function(solvedMatrix) inv <<- solvedMatrix
    getInverse <- function() inv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
    }
  
    #Q2
    ##Create a cacheSolve that computes the inverse of the special matrix returned by the function above 
    ##The function below computes the inverse of the special matrix returned by the  makecachematrix above
    cacheSolve <- function(x, ...) {
      ##Return a matrix that is the inverse of "x"
        inv <- x$getInverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
          }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
        }
