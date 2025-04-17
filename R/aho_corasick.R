#' Aho-Corasick multiple pattern matching algorithm using R
#'
#' @description Searches for all occurrences of multiple patterns in a given text using the Aho-Corasick algorithm.
#' @param text A character string representing the main text.
#' @param patterns A character vector containing multiple patterns to search for.
#' @return A list of integers indicating the starting positions of each match (1-based index).
aho_corasick <- function(text, patterns) {
  
  build_ac_trie <- function(patterns) {
    trie <- list()
    fail <- list()
    output <- list()
    
    root <- "root"
    trie[[root]] <- list()
    
    for (pattern in patterns) {
      current <- root
      for (char in unlist(strsplit(pattern, ""))) {
        if (!(char %in% names(trie[[current]]))) {
          new_state <- paste0(current, char)
          trie[[current]][[char]] <- new_state
          trie[[new_state]] <- list()
        }
        current <- trie[[current]][[char]]
      }
      output[[current]] <- pattern
    }
    
    queue <- c()
    for (char in names(trie[[root]])) {
      fail[[trie[[root]][[char]]]] <- root
      queue <- c(queue, trie[[root]][[char]])
    }
    
    while (length(queue) > 0) {
      state <- queue[1]
      queue <- queue[-1]
      
      for (char in names(trie[[state]])) {
        next_state <- trie[[state]][[char]]
        queue <- c(queue, next_state)
        
        fail_state <- fail[[state]]
        while (fail_state != root && !(char %in% names(trie[[fail_state]]))) {
          fail_state <- fail[[fail_state]]
        }
        
        fail[[next_state]] <- ifelse(char %in% names(trie[[fail_state]]), trie[[fail_state]][[char]], root)
      }
    }
    
    list(trie = trie, fail = fail, output = output)
  }
  
  search <- function(text, ac_trie) {
    matches <- list()  # liste pour stocker les motifs et leurs positions
    state <- "root"
    
    # Créer une liste pour stocker toutes les positions par motif
    result_list <- list()
    
    for (i in seq_along(unlist(strsplit(text, "")))) {
      char <- substr(text, i, i)
      while (state != "root" && !(char %in% names(ac_trie$trie[[state]]))) {
        state <- ac_trie$fail[[state]]
      }
      state <- ifelse(char %in% names(ac_trie$trie[[state]]), ac_trie$trie[[state]][[char]], "root")
      
      if (state %in% names(ac_trie$output)) {
        pattern_found <- ac_trie$output[[state]]
        position <- i - nchar(pattern_found) + 1
        
        # Ajouter le motif et sa position à la liste des résultats
        if (is.null(result_list[[pattern_found]])) {
          result_list[[pattern_found]] <- list()
        }
        result_list[[pattern_found]] <- c(result_list[[pattern_found]], position)
      }
    }
    
    # Fusionner les résultats sous forme de liste
    matches <- lapply(names(result_list), function(pattern) {
      list(pattern = pattern, positions = unlist(result_list[[pattern]]))
    })
    
    return(matches)
  }
  
  ac_trie <- build_ac_trie(patterns)
  return(search(text, ac_trie))
}