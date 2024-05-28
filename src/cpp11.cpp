// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// code.cpp
void fun();
extern "C" SEXP _quadroot_fun() {
  BEGIN_CPP11
    fun();
    return R_NilValue;
  END_CPP11
}
// collectdfc.cpp
writable::list collectdfc(SEXP x);
extern "C" SEXP _quadroot_collectdfc(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(collectdfc(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// quadpointsc.cpp
list quadpointsc(data_frame df);
extern "C" SEXP _quadroot_quadpointsc(SEXP df) {
  BEGIN_CPP11
    return cpp11::as_sexp(quadpointsc(cpp11::as_cpp<cpp11::decay_t<data_frame>>(df)));
  END_CPP11
}
// seq2.cpp
std::vector<int> seq2(int startx, int starty, int length, int n);
extern "C" SEXP _quadroot_seq2(SEXP startx, SEXP starty, SEXP length, SEXP n) {
  BEGIN_CPP11
    return cpp11::as_sexp(seq2(cpp11::as_cpp<cpp11::decay_t<int>>(startx), cpp11::as_cpp<cpp11::decay_t<int>>(starty), cpp11::as_cpp<cpp11::decay_t<int>>(length), cpp11::as_cpp<cpp11::decay_t<int>>(n)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_quadroot_collectdfc",  (DL_FUNC) &_quadroot_collectdfc,  1},
    {"_quadroot_fun",         (DL_FUNC) &_quadroot_fun,         0},
    {"_quadroot_quadpointsc", (DL_FUNC) &_quadroot_quadpointsc, 1},
    {"_quadroot_seq2",        (DL_FUNC) &_quadroot_seq2,        4},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_quadroot(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
