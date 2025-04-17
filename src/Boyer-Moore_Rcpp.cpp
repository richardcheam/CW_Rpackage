#include <Rcpp.h>
#include <vector>
#include <string>
#include <algorithm>

using namespace Rcpp;

// Prétraitement pour la Bad Character Rule
// [[Rcpp::export]]
void badCharacterHeuristic(const std::string &pattern, std::vector<int> &badChar) {
  int m = pattern.size();
  badChar.assign(256, -1); // 256 pour tous les caractères ASCII
  
  for (int i = 0; i < m; i++) {
    badChar[(unsigned char)pattern[i]] = i;
  }
}

// Prétraitement pour la Good Suffix Rule
// [[Rcpp::export]]
void goodSuffixHeuristic(const std::string &pattern, std::vector<int> &goodSuffix, std::vector<int> &suffix) {
  int m = pattern.size();
  suffix.assign(m, -1);
  goodSuffix.assign(m, m);
  
  int f = 0, g;
  
  // Suffixes
  suffix[m - 1] = m - 1;
  g = m - 1;
  for (int i = m - 2; i >= 0; i--) {
    if (i > g && suffix[i + m - 1 - f] < i - g) {
      suffix[i] = suffix[i + m - 1 - f];
    } else {
      if (i < g) g = i;
      f = i;
      while (g >= 0 && pattern[g] == pattern[g + m - 1 - f]) {
        g--;
      }
      suffix[i] = f - g;
    }
  }
  
  // Good suffix
  for (int i = 0; i < m; i++) {
    goodSuffix[i] = m;
  }
  
  int j = 0;
  for (int i = m - 1; i >= -1; i--) {
    if (i == -1 || suffix[i] == i + 1) {
      for (; j < m - 1 - i; j++) {
        if (goodSuffix[j] == m) {
          goodSuffix[j] = m - 1 - i;
        }
      }
    }
  }
  
  for (int i = 0; i <= m - 2; i++) {
    goodSuffix[m - 1 - suffix[i]] = m - 1 - i;
  }
}
//' Boyer-Moore pattern matching algorithm using R
//' @description Searches for all occurrences of the pattern in a given text using the Boyer-Moore algorithm.
//' @param text A character string representing the main text.
//' @param pattern A character vector containing a single patterns to search for.
//' @return A list of integers indicating the starting positions of each match (1-based index).
// [[Rcpp::export]]
std::vector<int> boyer_moore_search_Rcpp(std::string text, std::string pattern) {
  int n = text.size();
  int m = pattern.size();
  std::vector<int> result;
  
  if (m == 0 || n < m) return result;
  
  std::vector<int> badChar;
  std::vector<int> goodSuffix, suffix;
  
  badCharacterHeuristic(pattern, badChar);
  goodSuffixHeuristic(pattern, goodSuffix, suffix);
  
  int s = 0; // shift
  
  while (s <= n - m) {
    int j = m - 1;
    
    while (j >= 0 && pattern[j] == text[s + j]) {
      j--;
    }
    
    if (j < 0) {
      result.push_back(s + 1); // 1-based index
      s += goodSuffix[0];
    } else {
      int shift_bad = j - badChar[(unsigned char)text[s + j]];
      int shift_good = goodSuffix[j];
      s += std::max(1, std::max(shift_bad, shift_good));
    }
  }
  
  return result;
}
