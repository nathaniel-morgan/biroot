#include <cpp11.hpp>

using namespace cpp11;

[[cpp11::register]]
list quadpointsc(data_frame df) {
  doubles x = df["x"];
  doubles y = df["y"];
  
  int n = x.size();
  writable::list out(4);
  
  for(int i = 0; i < 4; i++) {
    int index1 = (i + 1) % n;
    
    writable::doubles newX(n);
    writable::doubles newY(n);
    for(int j = 0; j < n; j++) {
      newX[j] = (x[j] + x[index1]) / 2;
      newY[j] = (y[j] + y[index1]) / 2;
    }
    
    out[i] = writable::data_frame{
      "x"_nm = newX,
      "y"_nm = newY
    };
  }
  
  return out;
}
