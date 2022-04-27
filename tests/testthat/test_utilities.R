
test_that("stype abort basic checks", {
  # NOTE: see math operation tests in the test_{stype} files for non-generic
  # testing
  bad <- function() stype_abort("you been bad")
  real_bad <- function(x, op) {
    stop_invalid_math(x, op, class = "the_worst")
  }

  expect_error(bad(), class = "stype_err")
  expect_error(real_bad(nneg(), "~"),
    c("~ is not defined for stype class v_continuous_nonneg."),
    class = c("the_worst", "stype_invalid_math", "stype_err")
  )
})

test_that("print functions work", {
  x <- v_binary(c(TRUE))
  expect_equal(print_context(x), "")
  expect_equal(print_missing(x), "")
  expect_equal(print_numeric_summary(get_data_summary(x, "n"), "n"), "n = 1.000")
  expect_output(
    print_footer(x, list(proportion = list(label = "Proportion", printer = print_numeric_summary))),
    "Proportion = 1.000"
  )

  z <- v_binary(c(TRUE, NA), context = context(purpose = purpose(study_role = "outcome")))
  expect_match(print_missing(z), "Missing = 1")
  expect_output(
    print_footer(z, list(proportion = list(label = "Proportion", printer = print_numeric_summary))),
    "Proportion = 1.000; Missing = 1\nPurpose: outcome"
  )
})
