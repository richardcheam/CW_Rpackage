#include <Rcpp.h>
using namespace Rcpp;
#include<vector>


//' Naive pattern matching algorithm using C++
//'
//' @description Searches for all occurrences of a pattern in a given text using a brute-force approach.
//' @param texte A character string representing the main text.
//' @param motif A character string representing the pattern to search for.
//' @return A vector of integers indicating the starting positions of each match (1-based index).
// [[Rcpp::export]]
std::vector<int> naive_pattern_matching_Rcpp(std::string texte, std::string motif) {
  int n = texte.length();
  int m = motif.length();
  std::vector<int> positions;
  
  // Parcours du texte
  for (int i = 0; i <= n - m; i++) {
    int j;
    for (j = 0; j < m; j++) {
      if (texte[i + j] != motif[j]) {
        break;
      }
    }
    if (j == m) { // Si la boucle interne a terminé sans rupture, motif trouvé
      positions.push_back(i + 1); // Conversion en indexation 1-based pour R
    }
  }
  
  return positions;
}