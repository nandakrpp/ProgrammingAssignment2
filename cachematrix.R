# cachematrix.R
#
# contains two functions, that together, cache potenbtially time-consuming computations
# for calculating an inversed value of a matrix. In short, if the inversed value has 
# already existed, the scirpt will fetchs the value from the cache and return it as a 
# result, otherwise compute it.
# 
# To test whether this script is working, creat an invertable matrix as follows:
#         > myMatrix <- makeCacheMatrix(matrix(c(1:4), nrow=2, ncol=2))
# Then execute the cacheSolve() with parameter 'myMatrix':
#         > cacheSolve(myMatrix)       
# Return value:
#         [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# Then check if anther return value is fetched from the cache:
# > cacheSolve(myMatrix)    
# Return value:
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
# Modified from cachematrix_1_1.R
# Last modified: on 2015-01-24 at 09.17
# 



# The fist function creates a special "matrix" via four sub-functions i.e. setMatrix(),
# getMatrix(), setInverse() and getInverse(); then return a list of returning values
# from these sub-functions as the main result of the scrip.
makeCacheMatrix <- function(mt = matrix()) {
        
        ## Create an empty matrix for storing the result.
        ## Setting it to NULL is a preparation for the command 'iv <<- NULL' 
        ## in the the setMatrix() function for searching for the 'iv' value in the  
        ## environment of this makeCacheMatrix() function, which is the global 
        ## environment. For better explanation, see course forum thread id-364.
        iv <- NULL 
        
        
        ## Sets (in case a branded new matrix is parsed into this makeCacheMatrix) or
        ## resets the matrix (in case the parsed in matrix's value is exactly the same as 
        ## the one that has been recently tested). Thus, this setMatrix() function only 
        ## makes a difference to the execution of this code ONLY when the branded new matrix 
        ## is different from the existing one. In other words, this setMatrix() function is 
        ## ONLY neccessary if the current 'mt' value is NOT the same as the recently tested 
        ## 'mt' value.
        setMatrix <- function(y) {
                ### The  '<<-' assigning operator causes R to look for the names 'mt' and 'iv' 
                ### in this child's environemnt. If they don't exist, R search for them upstream 
                ### (line-by-line) and assign the found values from the parent environemnt to 
                ### to them. In this case, the environment is global.
                
                ### Stores the testing matrix parsed in via the argument of this 
                ### makeCacheMatrix() function
                mt <<- y 
                
                ### Prepares a clean matrix for storing the inversed value. 
                iv <<- NULL 
        }
        
        
        ## Fetched the matrix value from the above function
        getMatrix <- function() {                
                return(mt)
        }
        
        
        ## Sets inversed value to the matrix calculated in the cacheSolve() function
        setInverse <- function(inverse) {
                ## Again, the '<<-' assigning operator causes R to look for the name 
                ## ('iv' in this case) in the parent's environments of setmean. It 
                ## finds this name in makeCacheMatrix()'s environment and sets the  
                ## name's to the parameter parsed in via the argument of this setInverse()
                ## function. 
                
                ## Assignes the inversed matrix via mt$setInverse(iv) in 
                ## the cacheSolve() function
                iv <<- inverse 
                
        }
        
        
        ## Fetches the inversed matrix from the above function and returns it
        getInverse <- function() {
                ## What to return? It looks like nothing is defined in its 
                ## argument???? The answer is that this getInverse() function itself 
                ## is called in the global environment, where the cacheSolve() function
                ## exists -- the same environment that the makeCacheMatrix() is initiated.                 
                ## So R's upstream searching characteristic gets the 'iv' value from there 
                ## and thus parsed the 'iv' into this getInverse() function's argument.
                return(iv) 
        }
        
        
        ## Returns all 4 results created via the previous four functions.
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse) 
        ## To see what are in this child's environment, execute names(myMatrix) command. 
        ## The result is:
        ## [1] "setMatrix"  "getMatrix"  "setInverse" "getInverse"
        
}




# The second function calulates the inversed value of the special "matrix" created with the
# the first function. To skip the time-consuming computation, it first checks to see if the
# inversed value has already existed. If so, it gets the inversed value from the cache.
# Otherwise, it calculates the inversed value from the given/test matrix and set the value
# in the cache by calling the setInverse() function of the first function.

cacheSolve <- function(mt, ...) {
        
        # Sets the inversed result 'iv' to the existiong value obtained from the 
        # makeCacheMatrix() function 
        iv <- mt$getInverse()
        
        # Checks whether the value of 'iv' already exists in the cache.
        # If it does, report that as a text message to the console, and 
        # return the 'iv' value.
        if(!is.null(iv)) { 
                message("Aha ... getting cached data !")
                return(iv)
        }
        
        
        # Creates a temporary matrix for storing a branded new matrix value
        # obtained via getMarix() function in the makeCachemarix() function.
        data <- mt$getMatrix()
        
        # Then inverses the branded new matrix using instant R's solve() function. 
        # This result is a branded new inversed matrix. Not from cache.
        iv <- solve(data, ...)
        
        # Sets this branded new inversed matrix to the final result.
        mt$setInverse(iv)
        
        # Returns the final result.
        iv
}