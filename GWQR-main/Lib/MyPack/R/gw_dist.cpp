// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]
#include "library\RcppArmadillo\include\RcppArmadillo.h"
#include <math.h>
#include <RcppArmadilloForward.h>

//mat是矩阵
using namespace Rcpp;
using namespace arma;
using namespace stats;

#define POWDI(x, i) pow(x, i) 
/*POWDI(x, i)：计算x的i次方的宏定义，等同于pow(x, i) 函数。
GAUSSIAN、EXPONENTIAL、BISQUARE、TRICUBE、BOXCAR：
定义了几个常量，用于表示不同的核函数类型。
*/
#define GAUSSIAN 0
#define EXPONENTIAL 1
#define BISQUARE 2
#define TRICUBE 3
#define BOXCAR 4

//---------------------------------------------------------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------------------------------------------------------
// For debugging
void printVec(vec v) {
  int n = v.size();
  n = 10;

  for (int i=0; i<n; i++) {
    Rprintf("%f ", v(i));
  }
  Rprintf("\n");
}
// For debugging
void printMat(mat m)
{ // 循环遍历向量的元素，并使用Rprintf()函数打印每个元素的值。
  uword n = m.n_rows;
  uword p = m.n_cols;

  n = 10;
  if (m.n_rows < n)
  {
    n = m.n_rows;
  } // 获取矩阵的行数和列数，并使用双重循环遍历矩阵的元素。
  for (uword i=0; i<n; i++) {
    for (uword j=0; j<p; j++) {
      Rprintf("%f ", m(i, j));
    }
    Rprintf("\n");
  }
  Rprintf("\n");
}
//-------------------------------------------Distance metric calculations
const mat mSum(2, 1, fill::ones);
// mat mSum(2, 1, fill::ones)：创建了一个2行1列的矩阵，所有元素都初始化为1。
// distance matrix calculation
// coords must be a matrix with 2 columns
mat coordinate_rotate(mat coords, double theta)
{ // 用于将坐标矩阵按照给定的旋转角度进行旋转。
  int n = coords.n_rows;
  mat rotated_coords(n, 2);
  // 使用旋转公式将 coords 中的每个点进行旋转，并将结果存储在 rotated_coords 中。
  rotated_coords.col(0) = coords.col(0) * cos(theta) - coords.col(1) * sin(theta);
  rotated_coords.col(1) = coords.col(0) * sin(theta) + coords.col(1) * cos(theta);
  return rotated_coords;
}

//Eudclidean distance matrix
mat eu_dist_mat(mat in_locs, mat out_locs)
{ // 计算两组坐标之间的欧氏距离矩阵。
  int n_in = in_locs.n_rows;
  int n_out = out_locs.n_rows;
  mat eu_dist(n_in, n_out);
  int i = 0, j = 0;
  for (i = 0; i < n_in; i++)
  {
    for (j = 0; j < n_out; j++)
    {
      eu_dist(i,j) = sum(pow(in_locs.row(i) - out_locs.row(j),2));
    }
  }
  return sqrt(eu_dist);
}
//symmetrical distance matrix
mat eu_dist_smat(mat in_locs)
{ // 计算对称的欧氏距离矩阵。
  int n = in_locs.n_rows;
  mat eu_dist(n, n);
  for (int k = 0; k < n * n; k++)
  {
    int i = k / n, j = k % n;
    eu_dist(i, j) = sum(pow(in_locs.row(i) - in_locs.row(j), 2));
    eu_dist(j, i) = eu_dist(i, j);
  }
  return sqrt(eu_dist);
}

vec eu_dist_vec(mat in_locs, vec out_loc)
{ // 计算内部坐标矩阵与外部坐标向量之间的欧氏距离向量。
  int n_in = in_locs.n_rows;
  vec eu_dist(n_in);
  for (int i = 0; i < n_in; i++)
  {
    eu_dist(i) = sum(pow(in_locs.row(i) - trans(out_loc), 2));
  }
  return sqrt(eu_dist);
  // mat v_span(n_in, 1, fill::ones);
  // mat m_diff = in_locs - v_span * trans(out_loc);
  // return sqrt(m_diff % m_diff * mSum);
}

//Manhattan distance matrix
mat md_dist_mat(mat in_locs, mat out_locs)
{ // 计算两组坐标之间的曼哈顿距离矩阵。
  int n_in = in_locs.n_rows;
  int n_out = out_locs.n_rows;
  mat md_dist(n_in, n_out);
  for (int i = 0; i < n_in; i++)
  {
    for (int j = 0; j < n_out; j++)
    {
      md_dist(i, j) = sum(abs(in_locs.row(i) - out_locs.row(j)));
    }
  }
  return md_dist;
}

//symmetrical distance matrix
mat md_dist_smat(mat in_locs)
{ // 计算对称的曼哈顿距离矩阵。
  int n = in_locs.n_rows;
  mat md_dist(n, n);
  for (int i = 0; i < n; i++)
  {
    for (int j = i; j < n; j++)
    {
      md_dist(i, j) = sum(abs(in_locs.row(i) - in_locs.row(j)));
      md_dist(j, i) = md_dist(i, j);
    }
  }
  return md_dist;
}
vec md_dist_vec(mat in_locs, vec out_loc)
{ // 计算内部坐标矩阵与外部坐标向量之间的曼哈顿距离向量。
  int n_in = in_locs.n_rows;
  vec md_dist(n_in);
  for (int i = 0; i < n_in; i++)
  {
    md_dist(i) = sum(abs(in_locs.row(i) - trans(out_loc)));
  }
  return md_dist;
}

//Chebyshev distance matrix
mat cd_dist_mat(mat in_locs, mat out_locs)
{ // 计算两组坐标之间的切比雪夫距离矩阵。
  int n_in = in_locs.n_rows;
  int n_out = out_locs.n_rows;
  mat cd_dist(n_in, n_out);
  for (int i = 0; i < n_in; i++)
  {
    for (int j = i; j < n_out; j++)
    {
      cd_dist(i, j) = max(abs(in_locs.row(i) - out_locs.row(j)));
      cd_dist(j, i) = cd_dist(i, j);
    }
  }
  return cd_dist;
}

//symmetrical distance matrix
mat cd_dist_smat(mat in_locs)
{ // 计算对称的切比雪夫距离矩阵。
  int n = in_locs.n_rows;
  mat cd_dist(n, n);
  for (int i = 0; i < n; i++)
  {
    for (int j = i; j < n; j++)
    {
      cd_dist(i, j) = max(abs(in_locs.row(i) - in_locs.row(j)));
      cd_dist(j, i) = cd_dist(i, j);
    }
  }
  return cd_dist;
}
vec cd_dist_vec(mat in_locs, vec out_loc)
{ // 计算内部坐标矩阵与外部坐标向量之间的切比雪夫距离向量。
  int n_in = in_locs.n_rows;
  vec cd_dist(n_in);
  for (int i = 0; i < n_in; i++)
  {
    cd_dist(i) = max(abs(in_locs.row(i) - trans(out_loc)));
  }
  return cd_dist;
}

//Minkowski distance matrix
mat mk_dist_mat(mat in_locs, mat out_locs, double p)
{ // 计算两组坐标之间的闵可夫斯基距离矩阵。
  int n_in = in_locs.n_rows;
  int n_out = out_locs.n_rows;
  mat mk_dist(n_in, n_out);
  for (int i = 0; i < n_in; i++)
  {
    for (int j = 0; j < n_out; j++)
    {
      mk_dist(i, j) = pow(sum(pow(abs(in_locs.row(i) - out_locs.row(j)), p)), 1.0 / p);
    }
  }
  return mk_dist;
}
//symmetrical distance matrix
mat mk_dist_smat(mat in_locs, double p)
{ // 计算对称的闵可夫斯基距离矩阵。
  int n = in_locs.n_rows;
  mat mk_dist(n, n);
  for (int i = 0; i < n; i++)
  {
    for (int j = i; j < n; j++)
    {
      mk_dist(i, j) = pow(sum(pow(abs(in_locs.row(i) - in_locs.row(j)), p)), 1.0 / p);
      mk_dist(j, i) = mk_dist(i, j);
    }
  }
  return mk_dist;
}
vec mk_dist_vec(mat in_locs, vec out_loc, double p)
{ // 计算内部坐标矩阵与外部坐标向量之间的闵可夫斯基距离向量。
  int n_in = in_locs.n_rows;
  vec mk_dist(n_in);
  for (int i = 0; i < n_in; i++)
  {
    mk_dist(i) = pow(sum(pow(abs(in_locs.row(i) - trans(out_loc)), p)), 1.0 / p);
  }
  return mk_dist;
}
//Great circle distance
// Caculate the distance with a pair of lat and long
double sp_gcdist(double lon1, double lon2, double lat1, double lat2) {
  // 该函数是根据大圆（或称为球面）上的两点之间的曲线距离公式进行计算的，
  //考虑了地球的曲率和椭球形状，因此可以用于计算更准确的地理距离。
  double F, G, L, sinG2, cosG2, sinF2, cosF2, sinL2, cosL2, S, C;
  double w, R, a, f, D, H1, H2;
  double lat1R, lat2R, lon1R, lon2R, DE2RA;

  DE2RA = M_PI/180;
  a = 6378.137;              /* WGS-84 equatorial radius in km */
  f = 1.0/298.257223563;     /* WGS-84 ellipsoid flattening factor */

  if (fabs(lat1 - lat2) < DBL_EPSILON) {
    if (fabs(lon1 - lon2) < DBL_EPSILON) {
      return 0.0;
      /* Wouter Buytaert bug caught 100211 */
    } else if (fabs((fabs(lon1) + fabs(lon2)) - 360.0) < DBL_EPSILON) {
      return 0.0;
    }
  }
  lat1R = lat1*DE2RA;
  lat2R = lat2*DE2RA;
  lon1R = lon1*DE2RA;
  lon2R = lon2*DE2RA;

  F = ( lat1R + lat2R )/2.0;
  G = ( lat1R - lat2R )/2.0;
  L = ( lon1R - lon2R )/2.0;

  /*
   printf("%g %g %g %g; %g %g %g\n",  *lon1, *lon2, *lat1, *lat2, F, G, L);
   */

  sinG2 = POWDI( sin( G ), 2 );
  cosG2 = POWDI( cos( G ), 2 );
  sinF2 = POWDI( sin( F ), 2 );
  cosF2 = POWDI( cos( F ), 2 );
  sinL2 = POWDI( sin( L ), 2 );
  cosL2 = POWDI( cos( L ), 2 );

  S = sinG2*cosL2 + cosF2*sinL2;
  C = cosG2*cosL2 + sinF2*sinL2;

  w = atan( sqrt( S/C ) );
  R = sqrt( S*C )/w;

  D = 2*w*a;
  H1 = ( 3*R - 1 )/( 2*C );
  H2 = ( 3*R + 1 )/( 2*S );

  return D*( 1 + f*H1*sinF2*cosG2 - f*H2*cosF2*sinG2 );
}

// Calculate the distance vector between a point and a set of points, latitude and longitude required
vec sp_dists(mat dp, vec loc) {
  //计算一个点和一组点之间的距离矢量，所需的纬度和经度
  int N = dp.n_rows, j;
  vec dists(N, fill::zeros);
  double uout = loc(0), vout = loc(1);
  for (j = 0; j < N; j++) {
    dists(j) = sp_gcdist(dp(j, 0), uout, dp(j, 1), vout);
  }
  return dists;
}

// Equal to gw.dist, to be checked
// [[Rcpp::export]]
// 这段代码实现了一个多样性指标中的距离计算函数 gw_dist。
//该函数接受多个参数，根据参数的不同计算不同类型的距离矩阵。
mat gw_dist(mat dp, mat rp, int focus, double p, double theta, bool longlat, bool rp_given) {
  int ndp = dp.n_rows, nrp = rp.n_rows;
  int isFocus = focus > -1;
  mat dists;
  if (p != 2 && theta != 0 && !longlat) {
    dp = coordinate_rotate(dp, theta);
    rp = coordinate_rotate(rp, theta);
  }
  if (isFocus) {
    mat prp = trans(rp.row(focus));
    if (longlat) {
      return sp_dists(dp, prp);
    } else {
      if (p == 2.0)
        return eu_dist_vec(dp, prp);
      else if(p == -1.0)
        return cd_dist_vec(dp, prp);
      else if(p == 1.0)
        return md_dist_vec(dp, prp);
      else
        return mk_dist_vec(dp, prp, p);
    }
  } else {
    if (longlat) {
      mat dists(ndp, nrp, fill::zeros);
      for (int i = 0; i < nrp; i++) {
        dists.col(i) = sp_dists(dp, trans(rp.row(i)));
      }
      return trans(dists);
    } else {
      if (p == 2.0)
        return rp_given ? eu_dist_mat(dp, rp) : eu_dist_smat(dp);
      else if (p == -1.0)
        return rp_given ? cd_dist_mat(dp, rp) : cd_dist_smat(dp);
      else if (p == 1.0)
        return rp_given ? md_dist_mat(dp, rp) : md_dist_smat(dp);
      else
        return rp_given ? mk_dist_mat(dp, rp, p) : mk_dist_smat(dp, p);
    }
  }
}
