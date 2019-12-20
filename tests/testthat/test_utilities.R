testthat::context("Testing stype utilities")

test_that("print functions work", {
  x <- v_binary(c(TRUE))
  expect_equal(print_context(x), "")
  expect_equal(print_missing(x), "") 
  expect_equal(print_data_summary(x, "n", "n"), "n = 1.000")
  expect_output(
    print_footer(x, c(proportion = "Proportion")),
    "Proportion = 1.000"
  )
  
  z <- v_binary(c(TRUE, NA), context = context(purpose = purpose(study_role = "outcome")))
  expect_equal(print_missing(z), "\033[1m\033[35mMissing = 1.000\033[39m\033[22m") 
  expect_output(
    print_footer(z, c(proportion = "Proportion")), 
    "Proportion = 1.000; Missing = 1.000\nPurpose: outcome"
  )
})