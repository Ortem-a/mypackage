library(testthat)
library(mypackage)

#test_check("mypackage")

source(file = '../R/Max.R')

test_that('Test of Max()', {
  a = data.frame(c(1,  2,  3,  6, 11, 12, 13,  7,  8,  9, 10, 4,  5))

  result = Max(a)

  expect_type(result, 'double')
  expect_length(result, 1)
  expect_true(result == 13)
})
