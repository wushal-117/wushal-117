# 这段代码定义了一个函数"SetQuantile"，用于设置一个随机分位数。函数接受一个参数tau，
# 该参数在此函数中没有实际作用，只是作为一个输入参数。函数返回一个随机分位数，
# 该分位数是一个位于0和1之间的随机数（保留两位小数）。

#' @title Set a random quantile from 0 to 1
#' @describeIn It is to set a random quantile for a simulation if you have
#' no idea with setting quantile, return a tau quantile(numeric)
#' @param tau a input parameter with always TRUE
#' @import quantreg
#' @return \code{tau} the random quantile at round(x,2)
#' @export
#'
#' @examples
SetQuantile = function(tau=TRUE){
  c = runif(1)
  return(round(c,2))
}
