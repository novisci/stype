testthat::context("Testing v_binary class")

stype_tester(
  v_type         = "v_binary",
  canonical_type = "logical",
  generator      = function() { c(TRUE, FALSE, TRUE, FALSE, NA) },
  error_generators = list(function() { c("A", "B") })
)

test_that(
  "v_binary operations work",
  {
    x1 <- v_binary(c(TRUE, FALSE, TRUE, FALSE, NA))
    expect_s3_class(!x1, "v_binary")
    expect_equivalent(vctrs::vec_data(!x1), c(FALSE, TRUE, FALSE, TRUE, NA))
    expect_equal(any(x1, na.rm = TRUE), TRUE)
    expect_equal(all(x1[c(1, 3)], na.rm = TRUE), TRUE)
    
    expect_equal(sum(x1[1:4]), 2)
    expect_error(sum(x1[1:4], x1[1:2]))
  }
)
