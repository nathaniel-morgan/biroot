#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame iso_c(DataFrame df) {
  NumericVector value = df["value"];
  NumericVector x = df["x"];
  NumericVector y = df["y"];
  String id = as<CharacterVector>(df["id"])[0];
  // zero counts as positive, matching sgn() on the R side, so every corner
  // pattern falls into one of the 16 cases below
  int n = value.size();
  NumericVector signs(n);
  for (int i = 0; i < n; i++) signs[i] = (value[i] >= 0) ? 1.0 : -1.0;
  double midx = (x[0] + x[2]) / 2;
  double midy = (y[0] + y[1]) / 2;
  
  if (is_true(all(signs == NumericVector::create(1, 1, 1, 1)))) {
    return DataFrame::create(_["x"] = NA_REAL, _["y"] = NA_REAL, _["id"] = id);
  }
  if (is_true(all(signs == NumericVector::create(-1, -1, -1, -1)))) {
    return DataFrame::create(_["x"] = NA_REAL, _["y"] = NA_REAL, _["id"] = id);
  }
  if (is_true(all(signs == NumericVector::create(-1, 1, 1, 1)))) {
    return DataFrame::create(_["x"] = NumericVector::create(midx, x[0]), _["y"] = NumericVector::create(y[0], midy), _["id"] = id);
  }
  if (is_true(all(signs == NumericVector::create(1, -1, 1, 1)))) {
    return DataFrame::create(_["x"] = NumericVector::create(x[0], midx), _["y"] = NumericVector::create(midy, y[1]), _["id"] = id);
  }
  if (is_true(all(signs == NumericVector::create(1, 1, -1, 1)))) {
    return DataFrame::create(_["x"] = NumericVector::create(midx, x[2]), _["y"] = NumericVector::create(y[1], midy), _["id"] = id);
  }
  if (is_true(all(signs == NumericVector::create(1, 1, 1, -1)))) {
    return DataFrame::create(_["x"] = NumericVector::create(midx, x[2]), _["y"] = NumericVector::create(y[0], midy), _["id"] = id);
  }
  if (is_true(all(signs == NumericVector::create(-1, -1, 1, 1)))) {
    return DataFrame::create(_["x"] = NumericVector::create(midx, midx), _["y"] = NumericVector::create(y[0], y[1]), _["id"] = id);
  }
  if (is_true(all(signs == NumericVector::create(-1, 1, -1, 1)))) {
    return DataFrame::create(_["x"] = NumericVector::create(x[0], midx, midx, x[2]), _["y"] = NumericVector::create(midy, y[0], y[1], midy), _["id"] = CharacterVector::create(id, id, std::string(id) + "-2"));
  }
  if (is_true(all(signs == NumericVector::create(-1, 1, 1, -1)))) {
    return DataFrame::create(_["x"] = NumericVector::create(x[0], x[2]), _["y"] = NumericVector::create(midy, midy), _["id"] = id);
  }
  if (is_true(all(signs == NumericVector::create(1, -1, -1, 1)))) {
    return DataFrame::create(_["x"] = NumericVector::create(x[0], x[2]), _["y"] = NumericVector::create(midy, midy), _["id"] = id);
  }
  if (is_true(all(signs == NumericVector::create(1, -1, 1, -1)))) {
    return DataFrame::create(_["x"] = NumericVector::create(x[0], midx, midx, x[2]), _["y"] = NumericVector::create(midy, y[1], y[0], midy), _["id"] = CharacterVector::create(id, id, std::string(id) + "-2"));
  }
  if (is_true(all(signs == NumericVector::create(1, 1, -1, -1)))) {
    return DataFrame::create(_["x"] = NumericVector::create(midx, midx), _["y"] = NumericVector::create(y[0], y[1]), _["id"] = id);
  }
  if (is_true(all(signs == NumericVector::create(-1, -1, -1, 1)))) {
    return DataFrame::create(_["x"] = NumericVector::create(midx, x[2]), _["y"] = NumericVector::create(y[0], midy), _["id"] = id);
  }
  if (is_true(all(signs == NumericVector::create(-1, -1, 1, -1)))) {
    return DataFrame::create(_["x"] = NumericVector::create(midx, x[2]), _["y"] = NumericVector::create(y[1], midy), _["id"] = id);
  }
  if (is_true(all(signs == NumericVector::create(-1, 1, -1, -1)))) {
    return DataFrame::create(_["x"] = NumericVector::create(x[0], midx), _["y"] = NumericVector::create(midy, y[1]), _["id"] = id);
  }
  if (is_true(all(signs == NumericVector::create(1, -1, -1, -1)))) {
    return DataFrame::create(_["x"] = NumericVector::create(x[0], midx), _["y"] = NumericVector::create(midy, y[0]), _["id"] = id);
  }
  
  stop("internal error: unhandled sign pattern in iso_c()");
  return DataFrame::create(); // not reached
}
