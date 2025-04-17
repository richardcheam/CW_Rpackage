bad_character_heuristic <- function(pattern) {
  m <- nchar(pattern)
  badChar <- rep(-1, 256)
  pat_chars <- strsplit(pattern, "")[[1]]
  
  for (i in seq_len(m)) {
    ascii <- as.integer(charToRaw(pat_chars[i]))
    badChar[ascii + 1] <- i - 1
  }
  return(badChar)
}

good_suffix_heuristic <- function(pattern) {
  m <- nchar(pattern)
  pat <- strsplit(pattern, "")[[1]]
  
  suffix <- rep(-1, m)
  goodSuffix <- rep(m, m)  
  
  suffix[m] <- m - 1  
  f <- 0
  g <- m - 1
  
  if(m >= 2){
    for (i in rev(0:(m - 2))) {
      if (i > g && suffix[i + m - 1 - f + 1] < i - g) {
        suffix[i + 1] <- suffix[i + m - 1 - f + 1]
      } else {
        if (i < g) {
          g <- i
        }
        f <- i
        while (g >= 0 && pat[g + 1] == pat[g + m - 1 - f + 1]) {
          g <- g - 1
        }
        suffix[i + 1] <- f - g
      }
    }
  }
  
  j <- 0  
  for (i in seq(from = m - 1, to = -1, by = -1)) {
    if (i == -1 || suffix[i + 1] == i + 1) {
      while (j < m - 1 - i) {
        if (goodSuffix[j + 1] == m) {
          goodSuffix[j + 1] <- m - 1 - i
        }
        j <- j + 1
      }
    }
  }
  
  for (i in 0:(m - 2)) {
    pos <- m - 1 - suffix[i + 1]  
    goodSuffix[pos + 1] <- m - 1 - i
  }
  
  return(list(goodSuffix = goodSuffix, suffix = suffix))
}

#' Boyer-Moore pattern matching algorithm using R
#'
#' @description Searches for all occurrences of the pattern in a given text using the Boyer-Moore algorithm.
#' @param text A character string representing the main text.
#' @param pattern A character vector containing a single patterns to search for.
#' @return A list of integers indicating the starting positions of each match (1-based index).
boyer_moore_search_R <- function(text, pattern) {
  n <- nchar(text)
  m <- nchar(pattern)
  
  if (m == 0 || n < m) return(integer(0))
  
  badChar <- bad_character_heuristic(pattern)
  gs <- good_suffix_heuristic(pattern)
  goodSuffix <- gs$goodSuffix  
  
  text_chars <- strsplit(text, "")[[1]]
  pat_chars  <- strsplit(pattern, "")[[1]]
  
  result <- integer(0)
  
  s <- 0  
  
  while (s <= n - m) {
    j <- m - 1  
    
    while (j >= 0 && pat_chars[j + 1] == text_chars[s + j + 1]) {
      j <- j - 1
    }
    
    if (j < 0) {
      result <- c(result, s + 1)
      s <- s + goodSuffix[1]
    } else {
      ascii_val <- as.integer(charToRaw(text_chars[s + j + 1]))
      shift_bad <- j - badChar[ascii_val + 1]
      shift_good <- goodSuffix[j + 1]
      s <- s + max(1, max(shift_bad, shift_good))
    }
  }
  
  return(result)
}