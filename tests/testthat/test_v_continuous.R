testthat::context("Testing v_continuous class")

test_that(
  "v_continuous class behaves itself",
  {
    x1 <- v_continuous(rnorm(5))
    x2 <- v_continuous(rnorm(5))
    sx1 <- x1[1:2]
    expected_attrs <- c("internal_name", "data_summary", "context") 
    
    expect_s3_class(x1, "v_continuous")
    expect_s3_class(sx1, "v_continuous")
    expect_true(all(expected_attrs%in% names(attributes(x1))))
    expect_true(all(expected_attrs%in% names(attributes(sx1))))
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
