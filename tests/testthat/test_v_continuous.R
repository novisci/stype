testthat::context("Testing v_continuous class")

test_that(
  "v_continuous class behaves itself",
  {
    x1 <- v_continuous(rnorm(5))
    x2 <- v_continuous(rnorm(5))
    expect_s3_class(x1, "v_continuous")
    expect_s3_class(x1[1:2], "v_continuous")
    expect_s3_class(vctrs::vec_c(x1, x2), "v_continuous")
    expect_s3_class(c(x1, x2), "v_continuous")
  }
)


test_that(
  "v_continuous operations work",
  {
    x1 <- v_continuous(rnorm(5))
    expect_s3_class(x1 + 5, "v_continuous")
    # TODO: more checks
  }
)
