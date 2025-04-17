library(testthat)
library(CWRpackage)

### RETURNS ###

test_that("return is a list R",
          {
            text <- "abbabbajjkshers"
            patterns <- c("abba", "bjj", "jjk", "she", "her")
            result <- aho_corasick(text, patterns)
            expect_equal(is.list(result), TRUE)
          })
test_that("return is a list C++",
          {
            text <- "abbabbajjkshers"
            patterns <- c("abba", "bjj", "jjk", "she", "her")
            result <- aho_corasick_Rcpp(text, patterns)
            expect_equal(is.list(result), TRUE)
          })
test_that("return has correct structure R",
          {
            text <- "abbabbajjkshers"
            patterns <- c("abba", "bjj", "jjk", "she", "her")
            result <- aho_corasick(text, patterns)
            expect_true(all(sapply(result, function(x) "pattern" %in% names(x))))
            expect_true(all(sapply(result, function(x) "positions" %in% names(x))))
          })

test_that("return has correct structure C++",
          {
            text <- "abbabbajjkshers"
            patterns <- c("abba", "bjj", "jjk", "she", "her")
            result <- aho_corasick_Rcpp(text, patterns)
            expect_true(all(sapply(result, function(x) "pattern" %in% names(x))))
            expect_true(all(sapply(result, function(x) "positions" %in% names(x))))
          })


### MATCHING ###

test_that("correct matching of patterns C++",
          {
            text <- "abbabbabjjkshers"
            patterns <- c("abba", "bjj")
            result <- aho_corasick_Rcpp(text, patterns)
            # Checking the match for 'abba'
            abba_positions <- result[[which(sapply(result, function(x) x$pattern) == "abba")]]$positions
            expect_equal(abba_positions, c(1, 4))
            # Checking the match for 'bjj'
            bjj_positions <- result[[which(sapply(result, function(x) x$pattern) == "bjj")]]$positions
            expect_equal(bjj_positions, 8)
          })


test_that("correct matching of patterns R",
          {
            text <- "abbabbabjjkshers"
            patterns <- c("abba", "bjj")
            result <- aho_corasick(text, patterns)
            # Checking the match for 'abba'
            abba_positions <- result[[which(sapply(result, function(x) x$pattern) == "abba")]]$positions
            expect_equal(abba_positions, c(1, 4))
            # Checking the match for 'bjj'
            bjj_positions <- result[[which(sapply(result, function(x) x$pattern) == "bjj")]]$positions
            expect_equal(bjj_positions, 8)
          })

test_that("empty result for non-matching pattern R",
          {
            text <- "abbabbajjkshers"
            patterns <- c("xyz", "qwerty")
            result <- aho_corasick(text, patterns)
            expect_equal(length(result), 0)
          })

test_that("empty result for non-matching pattern C++",
          {
            text <- "abbabbajjkshers"
            patterns <- c("xyz", "qwerty")
            result <- aho_corasick_Rcpp(text, patterns)
            expect_equal(length(result), 0)
          })
