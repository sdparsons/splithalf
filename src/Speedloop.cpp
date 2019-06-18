# include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix Speedloop(NumericMatrix A, int x, NumericVector y){
  NumericVector tmp = y;
  int l = y.size();
  NumericMatrix tmpM = A;

  for(int i = 0; i < x; i++) {

    tmpM(_,i) = Rcpp::sample(tmp, l);
  }
  return tmpM;

}



