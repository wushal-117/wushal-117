#' geographical weighted quantile regression（地理加权分位数回归
#'该函数的具体实现过程是通过循环迭代不断更新带宽，并根据给定的核函数、带宽和分位数进行分位回归拟合。
#'同时，计算均方误差、决定系数和调整决定系数等指标，并返回结果。
#' @param dp
#' @param d distance matrix
#' @param X input
#' @param y output
#' @param h bandwidth
#' @param fig logic(0 or 1) whether to plot the MSE by bandwidth
#' @param tau quantile
#' @param kernel kernel function to choose
#' @param alpha the step to update bandwidth
#' @param tolar
#' @param process
#' @param adaptive
#' @param iteration
#'
#' @return \code{beta} regression coef
#' @export kernel.R
#'
#' @examples
# source("F:/demo1_vscode/cpp/GWQR-main/Lib/MyPack/R/Kernel.R")
library(quantreg)

# 样本点坐标
coordinates <- matrix(1:6, ncol = 2)
# 计算欧氏距离
d <- dist(coordinates)
# 将距离矩阵转换为矩阵形式
d_matrix <- as.matrix(d)
# 打印距离矩阵
# print(d_matrix)



dp <- matrix(1:6, ncol = 2)
X <- matrix(c(123,15,1,12,65,89), ncol = 3)  
Y <- matrix(c(13,19,10,124,85,8), ncol = 3)
print(X)
print(Y)



GWQR <- function(dp=F, d, X, y, h, fig=F, tau, kernel=FALSE, alpha=20,
                tolar=1e-3, process=T, adaptive=F,iteration=200,method='br'){
  mse = c()
  H = c()
  n = nrow(X)
  p = ncol(X)
  U = matrix(rep(0, n*n), n, n)
  V = matrix(rep(0, n*n), n, n)
  X_inter = cbind(1,X)
  # W = matrix(rep(1),1,9)

  cv = matrix(rep(0, n*(p+1)), n, p+1)

  for (it in 1:iteration){  #h from 1 to 100
    if (kernel==FALSE){
      stop('choose a kernel function')}
    for (j in 1:n){
      # for (i in 1:2){
      #   U[i,i] = abs(dp[i, 1] - dp[j, 1])
      #   V[i,i] = abs(dp[i, 2] - dp[j, 2])
      #   }
      # Xnew = cbind(X, U%*%X, V%*%X)
      d <- d[1:n, ]  # 确保 d 的长度与数据框 Y 和 X 的长度一致
      W = Kernel_(Kname=kernel, d=d[j, ], h=h)[1:n]

      # print(W)
      x_unique <- unique(X)

      fit = try(rq(Y ~ x_unique, weights=W, 0.5, method = method))
      # print("----------")
      # print(weights)
      # print("************")
      if ('try-error' %in% class(fit)){
        # print('yes')
        break}
      else{
        # cv[j, ] = fit$coefficients[1:(dim(X)[2]+1)]
        cv[j, ] = fit$coefficients
        }
      }
    Y.fitted = rowSums(cv*X_inter)
    # Y.fitted = cv*X
    error = (Y - Y.fitted)^2
    # H = X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
    # sigma_hat = sum((error)^2)/(n - 2*tr(H) + tr(t(H) %*% H))
    MSE = round(mean(error), 4)
    R2 = 1 - sum(error) / sum((Y - mean(Y))^2)
    adjR2 = 1 - ((n-1)/(n-p-1))*(1-R2)
    h = h + alpha
    # cat("h:", h, 'MSE:', MSE, 'iter:', it, '\n')
    mse[it] = MSE
    H[it] = h
    }
  if (fig){
    plot(H, mse, type='p', ylab='MSE', xlab='Bandwidth')
    fig = FALSE
    }

  return(list(beta=cv, MSE=MSE, h=h, R2 = R2, adjR2=adjR2))

  }


Kernel_ <- function(Kname=FALSE, d, dstar=FALSE, h=FALSE, dN=FALSE){
  if (Kname == "binary" | Kname=="Binary"){
    W = ifelse(d<=dstar, yes=1, no=0)  
  }

  if (Kname == "gaussian" | Kname=="Gaussian" | Kname == FALSE){
    W = exp(-0.5 * (d/h)^2)
  }

  if (Kname == "Exp" | Kname=="exp"){
    W = exp(-d/h)
  }

  if (Kname == "bi-square" | Kname=="Bi-square"){
    W = ifelse(d<h, yes=(1 - (d/h)^2)^2, no=0)
  }

  if (Kname == "rank" | Kname=="Rank"){
    W = exp(-rank(d)/h)
  }

  if (Kname == "adaptive" | Kname=="Adaptive"){
    W = ifelse(d<dN, yes=(1-(d/dN)^2)^2, no=0)
  }
  
  return(W)
}

# 演示调用该函数并进行示例计算。
# fit <- GWQR(dp=dp, d=d_matrix, X=X, y=Y, h=50, kernel = "bi-square", alpha=0.5, tau=0.5, iteration=20, fig=TRUE)
fit <- GWQR(dp=dp, d=d_matrix, X=X, y=Y, h=50, kernel = "bi-square", alpha=0.5, tau=0.5, iteration=20, fig=TRUE)

# print(fit)