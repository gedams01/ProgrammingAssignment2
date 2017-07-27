## makeCacheMatrix creates a special matrix that can cache its inverse
## x is the matrix
## InvX stores the initial value of Inverse of matrix, which is NULL

makeCacheMatrix <- function(x = matrix()) {
  InvX <- NULL
  set <- function(y) {
    x <<- y
    InvX <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) InvX <<- Inverse
  getInverse <- function() InvX
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## cachesolve comutes the inverse of matrix
## If inverse was already calculated, it gets the inverse from cache and skips computation
## Otherwise, it calculates the inverse and sets the value of the inverse 
## in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  InvX <- x$getInverse()
  if(!is.null(InvX)) {
    message("getting cached data")
    return(InvX)
  }
  data <- x$get()
  InvX <- solve(data, ...)
  x$setInverse(InvX)
  InvX
}  

x <- rbind(c(5, 1, 0),c(3,-1, 2),c(4, 0,-1)) 
x
solve(x)


my_matrix = makeCacheMatrix(x)
my_matrix$get()
my_matrix$getInverse()




Solution:

> makeCacheMatrix <- function(x = matrix()) {
+   InvX <- NULL
+   set <- function(y) {
+     x <<- y
+     InvX <<- NULL
+   }
+   get <- function() x
+   setInverse <- function(Inverse) InvX <<- Inverse
+   getInverse <- function() InvX
+   list(set = set, get = get,
+        setInverse = setInverse,
+        getInverse = getInverse)
+ }
> cacheSolve <- function(x, ...) {
+   InvX <- x$getInverse()
+   if(!is.null(InvX)) {
+     message("getting cached data")
+     return(InvX)
+   }
+   data <- x$get()
+   InvX <- solve(data, ...)
+   x$setInverse(InvX)
+   InvX
+ }
> x <- rbind(c(5, 1, 0),c(3,-1, 2),c(4, 0,-1))
> x
     [,1] [,2] [,3]
[1,]    5    1    0
[2,]    3   -1    2
[3,]    4    0   -1
> solve(x)
       [,1]    [,2]   [,3]
[1,] 0.0625  0.0625  0.125
[2,] 0.6875 -0.3125 -0.625
[3,] 0.2500  0.2500 -0.500
> my_matrix = makeCacheMatrix(x)
> my_matrix$get()
     [,1] [,2] [,3]
[1,]    5    1    0
[2,]    3   -1    2
[3,]    4    0   -1
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
       [,1]    [,2]   [,3]
[1,] 0.0625  0.0625  0.125
[2,] 0.6875 -0.3125 -0.625
[3,] 0.2500  0.2500 -0.500
> my_matrix$getInverse()
       [,1]    [,2]   [,3]
[1,] 0.0625  0.0625  0.125
[2,] 0.6875 -0.3125 -0.625
[3,] 0.2500  0.2500 -0.500
> m <- rbind(c(5,1,0),c(2,-1,3),c(0,4,-1))
> m
     [,1] [,2] [,3]
[1,]    5    1    0
[2,]    2   -1    3
[3,]    0    4   -1
> solve(m)
            [,1]        [,2]        [,3]
[1,]  0.20754717 -0.01886792 -0.05660377
[2,] -0.03773585  0.09433962  0.28301887
[3,] -0.15094340  0.37735849  0.13207547
> my_matrix$set(m)
> my_matrix$get()
     [,1] [,2] [,3]
[1,]    5    1    0
[2,]    2   -1    3
[3,]    0    4   -1
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
            [,1]        [,2]        [,3]
[1,]  0.20754717 -0.01886792 -0.05660377
[2,] -0.03773585  0.09433962  0.28301887
[3,] -0.15094340  0.37735849  0.13207547
> my_matrix$getInverse()
            [,1]        [,2]        [,3]
[1,]  0.20754717 -0.01886792 -0.05660377
[2,] -0.03773585  0.09433962  0.28301887
[3,] -0.15094340  0.37735849  0.13207547
> 