# // 总体而言，这段代码定义了一个函数gwqr_uv()，
# // 用于在矩阵X的末尾插入经过计算的矩阵U和V的结果。
# // 该函数使用了RcppArmadillo库来进行矩阵运算和数学计算。
library(RcppArmadillo)
library(Rcpp)

gwqr_uv <- function(dp, X) { # 用于在矩阵X的末尾插入经过计算的矩阵U和V的结果。
  n <- nrow(dp)
  p <- ncol(X)
  U <- matrix(0, n, n)
  V <- matrix(0, n, n)
  
  for (j in 1:n) {
    for (i in 1:n) {
      U[i, i] <- abs(dp[i, 1] - dp[j, 1])
      V[i, i] <- abs(dp[i, 2] - dp[j, 2])
    }
  }
  
  XX <- X
  X <- cbind(X, U %*% XX)
  X <- cbind(X, V %*% XX)
  
  return(X)
}
# 这段代码定义了一个名为"gwqr_reg"的函数。函数接受三个参数：dp、X和Y。
# 函数的主要功能是创建一个由n行n列的零矩阵A，并将对角线上的元素设置为1。其中，n是矩阵X的行数。
# 具体实现上，代码使用一个for循环遍历从1到n的索引，然后将矩阵A的对应位置设置为1，即A[i, i] <- 1。
# 最后，函数返回矩阵A。

gwqr_reg <- function(dp, X, Y) {
  n <- nrow(X)
  A <- matrix(0, n, n)
  
  for (i in 1:n) {
    A[i, i] <- 1
  }
  
  return(A)
}

# 接下来，代码定义了一个示例数据矩阵dp，它是一个2列的矩阵。然后定义了另外两个示例数据矩阵X和Y。
# 最后两行代码分别调用了函数"gwqr_uv"和"gwqr_reg"，并将结果分别赋值给变量result_uv和result_reg。
# 示例数据
dp <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), ncol = 2) 
X <- matrix(1:12, ncol = 3) 
Y <- c(1, 2, 3) 
print(dp)
print(X)
print(Y)
print("------------")

result_uv <- gwqr_uv(dp, X)
result_reg <- gwqr_reg(dp, X, Y)

print(result_uv)
print(result_reg)