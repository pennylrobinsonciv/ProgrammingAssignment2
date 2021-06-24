## cachematrix consists of makeCacheMatrix, a function that takes a matrix (square) and stores the
#data for use in cacheSolve - which takes the inverse of the stored matrix or
#returns the inverse if previously calculated

## makeCacheMatrix stores a matrix for cacheSolve to take inverse of - to use assign a matrix to it
#example - mat<-matrix(c(1,2,3,4),nrow=2,ncol=2)
#          mCm<-makeCacheMatrix(mat)  

makeCacheMatrix <- function(x = matrix()) {
			i<- NULL
			set <- function(y){
			x<<-y
			i<<-NULL
}
			get <- function()x
			setinv <- function(solve) i<<-solve 
			getinv <- function() i
			list(set = set, get=get, setinv=setinv, getinv=getinv)

}

## takes the matrix stored in makeCacheMatrix and returns the inverse; if matrix is not square 
#returns error. 
#	cS<-cacheSolve(mCm)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
			i <- x$getinv()
			if(!is.null(i)){
				message("getting cached data")
				return(i)
}
			data <- x$get()
			i <- solve(data,...)
			x$setinv(i)
			i
}			

