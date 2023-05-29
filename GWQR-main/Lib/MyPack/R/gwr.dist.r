# 函数首先对输入的数据点坐标进行检查，确保输入正确。然后根据是否提供回归点的坐标进行不同的处理。
# 如果提供了回归点的坐标（rp.locat存在），则检查回归点坐标的正确性，并将其转换为矩阵格式。
# 接下来，根据给定的参数调用名为"gw_dist"的函数计算距离矩阵，并将结果赋给变量"dists"。
# 如果未提供回归点的坐标，则将数据点的坐标作为回归点的坐标，并调用"gw_dist"函数计算距离矩阵，同样将结果赋给变量"dists"。
# 最后，函数返回距离矩阵"dists"。
  # export gwr.dist
gwr.dist <- function(dp.locat, rp.locat, focus = 0, p = 2, theta = 0, longlat = F)
{
  if (missing(dp.locat)||!is.numeric(dp.locat)||dim(dp.locat)[2]!=2)
    stop("Please input correct coordinates of data points")

  if (!missing(rp.locat)) {
    if (!is.numeric(rp.locaccpp/GWQR-main/Lib/MyPack/R/gwqr_reg.cpppp/GWQR-main/Lib/MyPack/R/gwr.dist.rt))
      stop("Please input correct coordinates of regression points")
    else
      rp.locat <- matrix(rp.locat, ncol = 2)
    if (focus < 0 || focus > length(rp.locat[,1]))
      stop("No regression point is fixed")
    dists <- gw_dist(dp.locat, rp.locat, focus - 1, ifelse(is.infinite(p), -1, p), theta, longlat, T)
  }
  else dists <- gw_dist(dp.locat, dp.locat, focus - 1, ifelse(is.infinite(p), -1, p), theta, longlat, F)
  dists
}
