# 函数首先根据给定的数据范围和点数，在设计矩阵x的最小值和最大值之间生成m个点。
# 然后，使用局部线性分位回归方法（rq函数）对每个点进行估计。在每个点上，通过计算自变量与当前点的距离，
# 使用核函数（默认为正态核函数）计算权重。然后，使用rq函数进行局部线性回归拟合，得到估计的系数。
# 最后，返回估计的参数（fv和dv）以及估计的样本点（points）和响应变量（y）。
# 这段代码还包含了一个使用示例，可以作为函数的调用参考。
#这段代码的目的是定义一个函数"llqr"，用于执行局部线性分位回归，并提供了相应的函数说明和参数说明。
#' @title Local linear Quantile Regression
#'
#' @description fit a local linear quantile regression with rq function,
#' @param x The design matrix
#' @param y The response variable
#' @param h The bandwidth parameter of a kernel function
#' @param tau The tau quantile (0 to 1)
#' @param m The number of points at which the function is to be estimated
#' @param kernel The kernel function with several types. \code{norm} is to chose the Normal kernal
#'
#' @return parameter estimation at sample points.
#' @export llqr
#'
#' @examples
llqr <- function(x, y, h, tau, m=200, kernel=FALSE){
  points = seq(min(x), max(x), length.out=m)
  fv = points
  dv = FALSE
  for (i in 1:length(points)){
    z = x - points[i]
    w = dnorm(z/h)
    fit = rq(y~x, tau = tau, weights = w, method = 'br')
    fv[i] = fit$coef[1]
    dv[i] = fit$coef[2]
  }

  return(list(points=points, y=y))
}



