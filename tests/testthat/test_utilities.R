
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
  expect_match(print_missing(z), "Missing = 1.000") 
  expect_output(
    print_footer(z, c(proportion = "Proportion")), 
    "Proportion = 1.000; Missing = 1.000\nPurpose: outcome"
  )
})