#' Naive Pattern Matching Algorithm
#'
#' @description Searches for all occurrences of a pattern in a given text using a brute-force approach.
#' @param texte A character string representing the main text.
#' @param motif A character string representing the pattern to search for.
#' @return A vector of integers indicating the starting positions of each match (1-based index).
naive_pattern_matching <- function(texte, motif) {
  n <- nchar(texte)      # Longueur du texte
  m <- nchar(motif)      # Longueur du motif
  positions <- c()       # Stocker les indices de correspondance
  
  # Parcourir le texte pour trouver des correspondances
  for (i in 0:(n - m)) {
    if (substr(texte, i + 1, i + m) == motif) {
      positions <- c(positions, i + 1)  # R enregistre les indices à partir de 1
    }
  }
  
  return(positions)
}