testthat::context("Testing v_binary class")

test_that(
  "v_binary class behaves itself",
  {
    x1 <- v_binary(c(TRUE, FALSE, TRUE, FALSE, NA))
    x2 <- v_binary(c(TRUE, TRUE, TRUE, FALSE, NA))
    expect_s3_class(x1, "v_binary")
    expect_s3_class(x1[1:2], "v_binary")
    expect_s3_class(vctrs::vec_c(x1, x2), "v_binary")
    expect_s3_class(c(x1, x2), "v_binary")
  }
)

test_that(
  "v_binary operations work",
  {
    x1 <- v_binary(c(TRUE, FALSE, TRUE, FALSE, NA))
    expect_s3_class(!x1, "v_binary")
    expect_equivalent(vctrs::vec_data(!x1), c(FALSE, TRUE, FALSE, TRUE, NA))
    expect_equal(any(x1, na.rm = TRUE), TRUE)
    expect_equal(all(x1[c(1, 3)], na.rm = TRUE), TRUE)
  }
)