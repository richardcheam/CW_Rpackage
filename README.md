# Academic project 

### Luc YAO, Léos COUTROT, Richard CHEAM

#### Evry Paris-Saclay University

### April 10, 2025

> [Package Presentation](#pp)

> [Quick Start](#qs)

> [Examples](#ex)

------------------------------------------------------------------------

<a id="pp"></a>

## Package Presentation

The `CW_Rpackage` R/Rcpp package is our own R/Rcpp packages as part of the **M2
Algorithmic courses in the Data Science master’s program at Université
d’Évry Paris-Saclay**.

This package provides implementations of various algorithmic strategies
in both R and Rcpp, including:

- **Naive pattern matching algorithm**
- **Aho-Corasick algorithm**: Multi-pattern matching algorithm 
- **Boyer-Moore**: Pattern matching algorithm induced with the bad character rule and the good suffixe rule

------------------------------------------------------------------------

<a id="qs"></a>

## Quick Start

### Prerequisites for Package Development

To develop and use the package, install the necessary dependencies:

``` r
install.packages(c("Rcpp", "RcppArmadillo", "devtools", "roxygen2", "testthat", "stringr"))
```

### Installing the Package from GitHub

To install the CW_Rpackage package, use:

``` r
devtools::install_github("aesthlu/CW_Rpackage")
```

Then, load the package:

``` r
library(CWRpackage)
```

Moreover, define a function to create string for testing below

``` r
create_long_string <- function(n, num_replacements, dictionary) {
  long_string <- paste0(sample(letters, n, replace = TRUE), collapse = "")
  
  # Generate positions with at least 8 characters apart
  positions <- numeric(num_replacements)
  positions[1] <- sample(1:(n - 5), 1)  # First position
  
  for (i in 2:num_replacements)
  {
    repeat 
    {
      pos <- sample(1:(n - 5), 1)  # Choose a random position
      if (all(abs(pos - positions[1:(i-1)]) >= 8)) # Ensure distance >= 8
      {  
        positions[i] <- pos
        break
      }
    }
  }
  positions <- sort(positions)
  
  # Replace substrings with dictionary words
  for (i in 1:num_replacements)
  {
    start_pos <- positions[i]
    word <- sample(dictionary, 1)  # Pick a random word
    substr(long_string, start_pos, start_pos + nchar(word) - 1) <- word
  }
  
  return(long_string)
}
```

------------------------------------------------------------------------

<a id="ex"></a>

## Examples
We first create a long string to compare our different algorithms.

``` r
n <- 1000
num_replacements <- 25
dictionary <- c("cats", "dogs", "car", "distance")
text <- create_long_string(n, num_replacements, dictionary)
```

### Naive pattern matching algorithm 

``` r
naive_pattern_matching(text, "cats")
```
    ## [1]  32  93 455

``` r
naive_pattern_matching_Rcpp(text, "cats")
```
    ## [1]  32  93 455


### Aho-Corasick algorithm

``` r

res <- aho_corasick(text, dictionary)
for (i in 1:length(res)) {
  cat("Pattern :", (res[[i]]$pattern), 
      "| Positions :", (res[[i]]$positions),"\n")
}
```

    ##  Pattern : distance | Positions : 12 76 127 180 194 668 718 751 
    ##  Pattern : cats | Positions : 32 93 455 
    ##  Pattern : dogs | Positions : 236 328 380 591 622 685 912 970 
    ##  Pattern : car | Positions : 500 727 738 779 794 832 

``` r
res <- aho_corasick(text, dictionary)
for (i in 1:length(res)) {
  cat("Pattern :", (res[[i]]$pattern), 
      "| Positions :", (res[[i]]$positions),"\n")
}
```

    ##  Pattern : distance | Positions : 12 76 127 180 194 668 718 751 
    ##  Pattern : cats | Positions : 32 93 455 
    ##  Pattern : dogs | Positions : 236 328 380 591 622 685 912 970 
    ##  Pattern : car | Positions : 500 727 738 779 794 832 


### Boyer-Moore algorithm 

``` r
boyer_moore_search_R(text, "cat")
```
    ##  [1]  32  93 455

``` r
boyer_moore_search_Rcpp(text, "cat")
```

    ##  [1]  32  93 455
