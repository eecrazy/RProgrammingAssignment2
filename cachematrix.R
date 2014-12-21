# 逆矩阵通常是费时费力的计算，缓存逆矩阵可能会比对其进行重复计算更有利（逆矩阵也有替代矩阵，我们不会在此进行讨论）。
# 你的作业是编写可以缓存逆矩阵的函数对。

# 1.    makeCacheMatri：此函数用于创建可缓存逆矩阵的特殊“矩阵”对象。
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

}

# 2.    cacheSolve：此函数用于计算上述makeCacheMatrix返回的特殊“矩阵”的逆矩阵。如果已经计算逆矩阵（且尚未更改矩阵）
# ，那么cachesolve将检索缓存中的逆矩阵。计算正方形矩阵的逆矩阵可以在R中通过solve函数来完成。例如，如果X是一个正方形的
# 可逆矩阵，那么solve(X)会返回其逆矩阵。
# 对于此作业，假设提供的矩阵始终可逆。

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix data")
                return(m)
        }
        data <- x$get_matrix()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}
