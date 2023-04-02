#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List find_closest_matches(NumericMatrix mat1, NumericMatrix mat2, int start_index, int end_index) {
  int n1 = mat1.nrow();
  int n2 = mat2.nrow();
  
  NumericVector min_distances(n1, R_PosInf);
  IntegerVector min_indices(n1, -1);
  
  for (int i = 0; i < n1; i++) {
    for (int j = start_index; j <= end_index; j++) {
      double dist_ij = 0.0;
      for (int k = 0; k < mat1.ncol(); k++) {
        double diff = double(mat1(i, k) - mat2(j, k));
        dist_ij += diff * diff;
      }
      double dist = sqrt(dist_ij);
      
      if (dist < min_distances[i]) {
        min_distances[i] = dist;
        min_indices[i] = j;
      }
    }
  }
  
  return List::create(Named("min_distances") = min_distances, Named("min_indices") = min_indices);
}
