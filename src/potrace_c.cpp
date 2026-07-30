#include <Rcpp.h>
#include <potracelib.h>
using namespace Rcpp;

// [[Rcpp::export]]
List potrace_polygons(std::string infile) {
  FILE *f = fopen(infile.c_str(), "rb");
  if (!f) stop("Cannot open file");
  potrace_bitmap_t *bm = pbm_read(f);
  fclose(f);
  if (!bm) stop("PBM read failed");
  
  potrace_param_t *param = potrace_param_default();
  potrace_state_t *st = potrace_trace(param, bm);
  
  if (!st || st->status != POTRACE_STATUS_OK) stop("Trace failed");
  
  List result;
  for (potrace_path_t *p = st->plist; p; p = p->next) {
    NumericMatrix coords(p->curve.n, 2);
    for (int i = 0; i < p->curve.n; i++) {
      coords(i, 0) = p->curve.c[i][0].x;
      coords(i, 1) = p->curve.c[i][0].y;
    }
    result.push_back(coords);
  }
  
  potrace_state_free(st);
  pbm_free(bm);
  potrace_param_free(param);
  
  return result;
}
