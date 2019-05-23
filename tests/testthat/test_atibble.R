context("Testing the atibble class")

testthat::test_that("", {
  atib <- atibble(
    name  = "atib",
    label = "a test atibble", 
    variable(rnorm(10), name = "x", short_label = "A numeric variable"),
    variable(as.logical(rbinom(10, 1, 0.5)), name = "y", short_label = "A boolean variable"),
    variable(rep(LETTERS[1:2], 5), name = "z", short_label = "A character variable"),
    variable(c(1L:9L, NA_integer_), name = "a", short_label = "An integer variable")
  )
  
  expect_s4_class(atib, "atibble")
  
  expect_s3_class(glance(atib), "tbl_df")
})
