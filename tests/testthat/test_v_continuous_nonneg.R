testthat::context("Testing v_continuous_nonneg class")

test_that(
  "v_continuous_nonneg class behaves itself",
  {
    x1 <- v_continuous_nonneg(abs(rnorm(5)))
    x2 <- v_continuous_nonneg(abs(rnorm(5)))
    expect_error(v_continuous_nonneg(c(-1, -1.1)))
    expect_true(inherits(x1, "v_continuous_nonneg"))
    expect_true(inherits(x1[1:2], "v_continuous_nonneg"))
    expect_true(inherits(vctrs::vec_c(x1, x2), "v_continuous_nonneg"))
    expect_true(inherits(c(x1, x2), "v_continuous_nonneg"))
  }
)

test_that(
  "v_continuous_nonneg operations work",
  {
    # TODO: more checks
  }
)
