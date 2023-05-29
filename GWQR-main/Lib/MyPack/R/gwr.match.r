# 函数首先对输入的数据进行检查，确保数据为Spatial对象或SpatialDataFrame对象，同时获取数据点的坐标信息。
# 然后，函数通过调用model.frame()函数和model.extract()函数从给定的数据和回归模型公式中提取出自变量和因变量。
# 接下来，函数返回一个列表，其中包括自变量矩阵x和因变量向量y。
# 请注意，该段代码可能依赖于其他函数（例如model.frame()和model.extract()），因此需要确保这些函数在代码执行前已经定义或导入。
#' @export gwr.match
gwr.match <- function(formula, data, approach="CV",kernel="bisquare",adaptive=FALSE, p=2, theta=0,
                      longlat=F,dMat,parallel.method=F,parallel.arg=NULL)
{
  ##Data points{
  if (is(data, "Spatial"))
  {
    dp.locat<-coordinates(data)
    data <- as(data, "data.frame")
  }
  else
  {
    stop("Given regression data must be Spatial*DataFrame")
  }
  #cat("This selection has been optimised by golden selection.\n")
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)

  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- model.extract(mf, "response")
  x <- model.matrix(mt, mf)
  dp.n<-nrow(data)
  return(list(x=x, y=y))
}
