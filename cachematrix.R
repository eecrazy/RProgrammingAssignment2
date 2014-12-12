# 逆矩阵通常是费时费力的计算，缓存逆矩阵可能会比对其进行重复计算更有利（逆矩阵也有替代矩阵，我们不会在此进行讨论）。
# 你的作业是编写可以缓存逆矩阵的函数对。

# 编写以下函数：
# 1.	makeCacheMatri：此函数用于创建可缓存逆矩阵的特殊“矩阵”对象。
# 2.	cacheSolve：此函数用于计算上述makeCacheMatrix返回的特殊“矩阵”的逆矩阵。如果已经计算逆矩阵（且尚未更改矩阵）
# ，那么cachesolve将检索缓存中的逆矩阵。计算正方形矩阵的逆矩阵可以在R中通过solve函数来完成。例如，如果X是一个正方形的
# 可逆矩阵，那么solve(X)会返回其逆矩阵。
# 对于此作业，假设提供的矩阵始终可逆。
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# 第一个函数makeCacheMatrix创建了一个特殊的“矩阵”，实际上它是包含具有以下用途的函数的列表
# 1.	设置矩阵值
# 2.	获取矩阵值
# 3.	计算逆矩阵
# 4.	获取逆矩阵
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set_matrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        get_matrix <- function() x

        set_inverse <- function(inverse) m <<- inverse
        
        get_inverse <- function() m
        
        list(set_matrix = set_matrix,
        	 get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

	solve(X)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached matrix data")
                return(m)
        }
        data <- x$get_matrix()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}








# 第一个函数makeVector创建了一个特殊的“向量”，实际上它是包含具有以下用途的函数的列表
# 1.	设置向量值
# 2.	获取向量值
# 3.	设置平均值
# 4.	获取平均值
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
# 以下函数计算出了上述函数创建的特殊“向量”的平均值。但是，它会首先查看是否已经计算了平均值。
# 如果是这种情况，那么它会从缓存中获取平均值并跳过计算。否则，它会计算数据的平均值并
# 通过setmean函数在缓存中设置平均值。
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