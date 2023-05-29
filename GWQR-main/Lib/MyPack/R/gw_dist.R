library(Rcpp)
library(RcppArmadillo)

# [[Rcpp::depends(RcppArmadillo)]]
# [[Rcpp::plugins(openmp)]]
# //matrix是矩阵
# For debugging
printVec <- function(v) {
  n <- length(v)
  n <- min(n, 10)
  
  for (i in 1:n) {
    cat(v[i], " ")
  }
  cat("\n")
}

# For debugging
# // 循环遍历向量的元素，并使用Rprintf()函数打印每个元素的值。
printMat <- function(m) {
  n <- nrow(m)
  p <- ncol(m)
  
  n <- min(n, 10)
#   // 获取矩阵的行数和列数，并使用双重循环遍历矩阵的元素。
  for (i in 1:n) {
    for (j in 1:p) {
      cat(m[i, j], " ")
    }
    cat("\n")
  }
  cat("\n")
}

# Distance metric calculations
mSum <- matrix(1, nrow = 2, ncol = 1)

# Coordinate rotation
# // 用于将坐标矩阵按照给定的旋转角度进行旋转。
coordinate_rotate <- function(coords, theta) {
  n <- nrow(coords)
  rotated_coords <- matrix(0, nrow = n, ncol = 2)
#   // 使用旋转公式将 coords 中的每个点进行旋转，并将结果存储在 rotated_coords 中。
  rotated_coords[, 1] <- coords[, 1] * cos(theta) - coords[, 2] * sin(theta)
  rotated_coords[, 2] <- coords[, 1] * sin(theta) + coords[, 2] * cos(theta)
  
  return(rotated_coords)
}


# Euclidean distance matrix
# // 计算两组坐标之间的欧氏距离矩阵。
eu_dist_mat <- function(in_locs, out_locs) {
  n_in <- nrow(in_locs)
  n_out <- nrow(out_locs)
  eu_dist <- matrix(0, nrow = n_in, ncol = n_out)
  
  for (i in 1:n_in) {
    for (j in 1:n_out) {
      eu_dist[i, j] <- sum((in_locs[i,] - out_locs[j,])^2)
    }
  }
  
  return(sqrt(eu_dist))
}

# Symmetrical distance matrix
# // 计算对称的欧氏距离矩阵。
eu_dist_smat <- function(in_locs) {
  n <- nrow(in_locs)
  eu_dist <- matrix(0, nrow = n, ncol = n)
  
  for (k in 1:(n * n)) {
    i <- (k - 1) %/% n + 1
    j <- (k - 1) %% n + 1
    
    eu_dist[i, j] <- sum((in_locs[i,] - in_locs[j,])^2)
    eu_dist[j, i] <- eu_dist[i, j]
  }
  
  return(sqrt(eu_dist))
}

# Euclidean distance vector
# // 计算内部坐标矩阵与外部坐标向量之间的欧氏距离向量。
eu_dist_vec <- function(in_locs, out_loc) {
  n_in <- nrow(in_locs)
  eu_dist <- rep(0, n_in)
  
  for (i in 1:n_in) {
    eu_dist[i] <- sum((in_locs[i,] - out_loc)^2)
  }
  
  return(sqrt(eu_dist))
  # Alternative implementation
  # v_span <- matrix(1, nrow = n_in, ncol = 1)
  # m_diff <- in_locs - v_span %*% t(out_loc)
  # return(sqrt(m_diff * m_diff %*% mSum))
  # 创建一个大小为n_in行1列的矩阵v_span，并将其所有元素设置为1。
  # 计算矩阵m_diff，它等于in_locs减去v_span与out_loc的转置的矩阵乘积。
  # 计算m_diff与自身的逐元素乘积，并对结果进行矩阵乘积运算，得到一个标量值。
  # 最后，返回该标量值的平方根。
}

# Manhattan distance matrix
# // 计算两组坐标之间的曼哈顿距离矩阵。
md_dist_mat <- function(in_locs, out_locs) {
  n_in <- nrow(in_locs)
  n_out <- nrow(out_locs)
  md_dist <- matrix(0, nrow = n_in, ncol = n_out)
  
  for (i in 1:n_in) {
    for (j in 1:n_out) {
      md_dist[i, j] <- sum(abs(in_locs[i,] - out_locs[j,]))
    }
  }
  
  return(md_dist)
}

# Symmetrical Manhattan distance matrix
# // 计算对称的曼哈顿距离矩阵。
md_dist_smat <- function(in_locs) {
  n <- nrow(in_locs)
  md_dist <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in i:n) {
      md_dist[i, j] <- sum(abs(in_locs[i,] - in_locs[j,]))
      md_dist[j, i] <- md_dist[i, j]
    }
  }
  
  return(md_dist)
}

# Manhattan distance vector
# // 计算内部坐标矩阵与外部坐标向量之间的曼哈顿距离向量。
md_dist_vec <- function(in_locs, out_loc) {
  n_in <- nrow(in_locs)
  md_dist <- rep(0, n_in)
  
  for (i in 1:n_in) {
    md_dist[i] <- sum(abs(in_locs[i,] - t(out_loc)))
  }
  
  return(md_dist)
}

# Chebyshev distance matrix
# // 计算两组坐标之间的切比雪夫距离矩阵。
cd_dist_mat <- function(in_locs, out_locs) {
  n_in <- nrow(in_locs)
  n_out <- nrow(out_locs)
  cd_dist <- matrix(0, nrow = n_in, ncol = n_out)
  
  for (i in 1:n_in) {
    for (j in i:n_out) {
      cd_dist[i, j] <- max(abs(in_locs[i,] - out_locs[j,]))
      cd_dist[j, i] <- cd_dist[i, j]
    }
  }
  
  return(cd_dist)
}

# Symmetrical Chebyshev distance matrix
# // 计算对称的切比雪夫距离矩阵。
cd_dist_smat <- function(in_locs) {
  n <- nrow(in_locs) 
  cd_dist <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in i:n) {
      cd_dist[i, j] <- max(abs(in_locs[i,] - in_locs[j,]))
      cd_dist[j, i] <- cd_dist[i, j]
    }
  }
  
  return(cd_dist)
}

# Chebyshev distance vector
# // 计算内部坐标矩阵与外部坐标向量之间的切比雪夫距离向量。
cd_dist_vec <- function(in_locs, out_loc) {
  n_in <- nrow(in_locs)
  cd_dist <- rep(0, n_in)
  
  for (i in 1:n_in) {
    cd_dist[i] <- max(abs(in_locs[i,] - t(out_loc)))
  }
  
  return(cd_dist)
}



# Minkowski distance matrix
# // 计算两组坐标之间的闵可夫斯基距离矩阵。
mk_dist_mat <- function(in_locs, out_locs, p) {
  n_in <- nrow(in_locs)
  n_out <- nrow(out_locs)
  mk_dist <- matrix(0, nrow = n_in, ncol = n_out)
  
  for (i in 1:n_in) {
    for (j in 1:n_out) {
      mk_dist[i, j] <- sum(abs(in_locs[i,] - out_locs[j,])^p)^(1/p)
    }
  }
  
  return(mk_dist)
}

# Symmetrical Minkowski distance matrix
# // 计算对称的闵可夫斯基距离矩阵。
mk_dist_smat <- function(in_locs, p) {
  n <- nrow(in_locs)
  mk_dist <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in i:n) {
      mk_dist[i, j] <- sum(abs(in_locs[i,] - in_locs[j,])^p)^(1/p)
      mk_dist[j, i] <- mk_dist[i, j]
    }
  }
  
  return(mk_dist)
}

# Minkowski distance vector
# // 计算内部坐标矩阵与外部坐标向量之间的闵可夫斯基距离向量。
mk_dist_vec <- function(in_locs, out_loc, p) {
  n_in <- nrow(in_locs)
  mk_dist <- rep(0, n_in)
  
  for (i in 1:n_in) {
    mk_dist[i] <- sum(abs(in_locs[i,] - t(out_loc))^p)^(1/p)
  }
  
  return(mk_dist)
}


# Great circle distance
# Calculate the distance with a pair of lat and long
sp_gcdist <- function(lon1, lon2, lat1, lat2) {
#   // 该函数是根据大圆（或称为球面）上的两点之间的曲线距离公式进行计算的，
#   //考虑了地球的曲率和椭球形状，因此可以用于计算更准确的地理距离。
  DE2RA <- pi / 180
  a <- 6378.137              # WGS-84 equatorial radius in km 赤道半径
  f <- 1.0 / 298.257223563   # WGS-84 ellipsoid flattening factor

  if (abs(lat1 - lat2) < .Machine$double.eps) {
    if (abs(lon1 - lon2) < .Machine$double.eps) {
      return(0.0)
    } else if (abs((abs(lon1) + abs(lon2)) - 360.0) < .Machine$double.eps) {
      return(0.0)
    }
  }

  lat1R <- lat1 * DE2RA
  lat2R <- lat2 * DE2RA
  lon1R <- lon1 * DE2RA
  lon2R <- lon2 * DE2RA

  F <- (lat1R + lat2R) / 2.0
  G <- (lat1R - lat2R) / 2.0
  L <- (lon1R - lon2R) / 2.0

  sinG2 <- sin(G) ^ 2
  cosG2 <- cos(G) ^ 2
  sinF2 <- sin(F) ^ 2
  cosF2 <- cos(F) ^ 2
  sinL2 <- sin(L) ^ 2
  cosL2 <- cos(L) ^ 2

  S <- sinG2 * cosL2 + cosF2 * sinL2
  C <- cosG2 * cosL2 + sinF2 * sinL2

  w <- atan(sqrt(S / C))
  R <- sqrt(S * C) / w

  D <- 2 * w * a
  H1 <- (3 * R - 1) / (2 * C)
  H2 <- (3 * R + 1) / (2 * S)

  return(D * (1 + f * H1 * sinF2 * cosG2 - f * H2 * cosF2 * sinG2))
}

# Calculate the distance vector between a point and a set of points, latitude and longitude required
sp_dists <- function(dp, loc) {
#   //计算一个点和一组点之间的距离矢量，所需的纬度和经度
  N <- nrow(dp)
  dists <- rep(0, N)
  uout <- loc[1]
  vout <- loc[2]
  
  for (j in 1:N) {
    dists[j] <- sp_gcdist(dp[j, 1], uout, dp[j, 2], vout)
  }
  
  return(dists)
}


# Equal to gw.dist, to be checked
# // 这段代码实现了一个多样性指标中的距离计算函数 gw_dist。
# //该函数接受多个参数，根据参数的不同计算不同类型的距离矩阵。
gw_dist <- function(dp, rp, focus, p, theta, longlat, rp_given) {
  ndp <- nrow(dp)
  nrp <- nrow(rp)
  isFocus <- focus > -1
  
  if (p != 2 && theta != 0 && !longlat) {
    dp <- coordinate_rotate(dp, theta)
    rp <- coordinate_rotate(rp, theta)
  }
  
  if (isFocus) {
    prp <- t(rp[focus, ])
    if (longlat) {
      return(sp_dists(dp, prp))
    } else {
      if (p == 2.0)
        return(eu_dist_vec(dp, prp))
      else if (p == -1.0)
        return(cd_dist_vec(dp, prp))
      else if (p == 1.0)
        return(md_dist_vec(dp, prp))
      else
        return(mk_dist_vec(dp, prp, p))
    }
  } else {
    if (longlat) {
      dists <- matrix(0, nrow = ndp, ncol = nrp)
      for (i in 1:nrp) {
        dists[, i] <- sp_dists(dp, t(rp[i, ]))
      }
      return(t(dists))
    } else {
      if (p == 2.0)
        return(if (rp_given) eu_dist_mat(dp, rp) else eu_dist_smat(dp))
      else if (p == -1.0)
        return(if (rp_given) cd_dist_mat(dp, rp) else cd_dist_smat(dp))
      else if (p == 1.0)
        return(if (rp_given) md_dist_mat(dp, rp) else md_dist_smat(dp))
      else
        return(if (rp_given) mk_dist_mat(dp, rp, p) else mk_dist_smat(dp, p))
    }
  }
}