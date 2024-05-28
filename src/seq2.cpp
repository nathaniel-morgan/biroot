#include "cpp11.hpp"
using namespace cpp11;


#include <iostream>
#include <vector>
#include <algorithm>

[[cpp11::register]]
std::vector<int> seq2(int startx, int starty, int length, int n) {
  std::vector<int> nei;
  
  // Calculate the bounds for starty
  int beginy = std::max(0, starty - length);
  int endy = std::min(n, starty + length);
  
  for (int i = beginy; i <= endy; ++i) {
    // Calculate the bounds for the current iteration
    int lbound = std::max(1 + n * i, startx - length + n * i);
    int rbound = std::min(n + n * i, std::min(startx + length + n * i, n * n));
    
    if (lbound < rbound) {
      // Append values to the nei vector
      for (int j = lbound; j <= rbound; ++j) {
        nei.push_back(j);
      }
    }
  }
  
  return nei;
}

