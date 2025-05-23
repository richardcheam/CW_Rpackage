# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

badCharacterHeuristic <- function(pattern, badChar) {
    invisible(.Call(`_CWRpackage_badCharacterHeuristic`, pattern, badChar))
}

goodSuffixHeuristic <- function(pattern, goodSuffix, suffix) {
    invisible(.Call(`_CWRpackage_goodSuffixHeuristic`, pattern, goodSuffix, suffix))
}

#' Boyer-Moore pattern matching algorithm using R
#' @description Searches for all occurrences of the pattern in a given text using the Boyer-Moore algorithm.
#' @param text A character string representing the main text.
#' @param pattern A character vector containing a single patterns to search for.
#' @return A list of integers indicating the starting positions of each match (1-based index).
boyer_moore_search_Rcpp <- function(text, pattern) {
    .Call(`_CWRpackage_boyer_moore_search_Rcpp`, text, pattern)
}

#' Aho-Corasick multiple pattern matching algorithm using C++
#'
#' @description Searches for all occurrences of multiple patterns in a given text using the Aho-Corasick algorithm.
#' @param text A character string representing the main text.
#' @param patterns A character vector containing multiple patterns to search for.
#' @return A list of integers indicating the starting positions of each match (1-based index).
aho_corasick_Rcpp <- function(text, patterns) {
    .Call(`_CWRpackage_aho_corasick_Rcpp`, text, patterns)
}

#' Naive pattern matching algorithm using C++
#'
#' @description Searches for all occurrences of a pattern in a given text using a brute-force approach.
#' @param texte A character string representing the main text.
#' @param motif A character string representing the pattern to search for.
#' @return A vector of integers indicating the starting positions of each match (1-based index).
naive_pattern_matching_Rcpp <- function(texte, motif) {
    .Call(`_CWRpackage_naive_pattern_matching_Rcpp`, texte, motif)
}

