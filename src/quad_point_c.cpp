#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List quad_point_c(DataFrame df) {
  NumericVector x = df["x"];
  NumericVector y = df["y"];
  
  int n = x.size();
  List out(4);
  
  CharacterVector labels = CharacterVector::create("tl", "tr", "br", "bl");
  
  for(int i = 0; i < 4; i++) {
    int index1 = (i + 1) % n;
    
    NumericVector newX(n);
    NumericVector newY(n);
    CharacterVector newLabels(n, labels[i]);
    
    for(int j = 0; j < n; j++) {
      newX[j] = (x[j] + x[index1]) / 2;
      newY[j] = (y[j] + y[index1]) / 2;
    }
    
    out[i] = DataFrame::create(Named("x") = newX, Named("y") = newY, Named("parent_position") = newLabels);
  }
  
  return out;
}
