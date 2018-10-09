#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericMatrix Speedloop(NumericMatrix A, int x, NumericVector y){
  NumericVector tmp = y;
  NumericMatrix tmpM = A;

  for(int i = 0; i < x; i++) {
    std::random_shuffle(tmp.begin(), tmp.end());
    tmpM(_,i) = tmp;
  }
  return tmpM;

}



