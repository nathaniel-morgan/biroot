#include <cpp11.hpp>

using namespace cpp11;

// Helper function to recursively collect data frames
writable::list collect_dfs(SEXP x, writable::list result) {
  // Check if x is a data frame
  if (TYPEOF(x) == VECSXP && Rf_inherits(x, "data.frame")) {
    result.push_back(x); // Add the data frame to the result list
  }
  // Check if x is a list and recursively collect data frames
  else if (TYPEOF(x) == VECSXP) {
    list lst = as_cpp<list>(x);
    for (int i = 0; i < lst.size(); ++i) {
      result = collect_dfs(lst[i], result); // Recursively collect data frames
    }
  }
  return result;
}

// Exported function to be called from R
[[cpp11::register]]
writable::list collectdfc(SEXP x) {
  writable::list result = {}; // Initialize an empty list
  return collect_dfs(x, result); // Start the recursive collection process
}


