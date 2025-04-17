library(testthat)
library(CWRpackage)



### RETURNS ###

test_that("return is a number R",
          {
            text <- "abbabbajjkshers"
            pattern <- "abba"
            result <- boyer_moore_search_R(text, pattern)
            expect_equal(is.double(result), TRUE)
          })

test_that("return is a number C++",
          {
            text <- "abbabbajjkshers"
            pattern <- "abba"
            result <- boyer_moore_search_Rcpp(text, pattern)
            expect_equal(is.integer(result), TRUE)
          })



### MATCHING ###

test_that("boyer_moore: version R et C++ retournent les mêmes positions (correctes)", {
  text <- "abcaddadcbcdabadcabcaddabdcbcdabdcabdcabcaddabcaddabcadd"
  pattern <- "abcadd"
  
  result_r <- boyer_moore_search_R(text, pattern)
  result_cpp <- boyer_moore_search_Rcpp(text, pattern)
  
  expect_type(result_r, "double")
  expect_type(result_cpp, "integer")
  
  expect_equal(result_r, result_cpp)
  expect_equal(result_r, c(1, 18, 39, 45, 51))
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