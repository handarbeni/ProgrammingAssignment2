## To find inverse of a matrix usually requires costly computation
## These functions below will cache the inverted matrix so if the inverse function
## is called for the same matrix, it will return the cache matrix instead of recompute 
## the inverted matrix. It will reduce the computation cost

## This function below contains 4 function
## set : to set the value of the matrix
## get : to get the value of the matrix
## setInverse : to set the value of the inverted matrix
## getInverse : to get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function below is used to return the inverted matrix
## The return value of the function is the cache if the inverse is already computed
## But if the inverse is not computed before, it will call the inverse function
## And compute the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invertedM <- x$getInverse()
        if(!is.null(invertedM)){ ## check whether the inverted matrix is already computed or not 
        	message("getting chaced data")
        	return(invertedM) ## return the cache
        }
        data <- x$get()
        invertedM <- solve(data, ...) ## compute the inverse
        x$setInverse(invertedM)
        invertedM
}
