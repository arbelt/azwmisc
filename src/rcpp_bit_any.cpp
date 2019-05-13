#include <Rcpp.h>

using namespace Rcpp;

//' bit_any
//'
//' Aggregate integer vector using bitwise or.
//'
// [[Rcpp::export]]
int bit_any(IntegerVector xs){
  int out = 0;

  int n = xs.size();

  for (int i = 0; i < n; i++){
    out = out | xs[i];
  }

  return out;
}
