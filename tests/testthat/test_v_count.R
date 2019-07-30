testthat::context("Testing v_count class")

test_that(
  "v_count class behaves itself",
  {
    x1 <- v_count(c(0L, 1L, 2L, NA_integer_))
    sx1 <- x1[1:2]
    expected_attrs <- c("internal_name", "data_summary", "context")     
    
    expect_s3_class(x1, "v_count")
    expect_true(all(expected_attrs %in% names(attributes(x1))))
    expect_true(all(expected_attrs %in% names(attributes(sx1))))
    expect_equivalent(min(x1, na.rm = TRUE), v_count(0L))
    expect_equivalent(max(x1, na.rm = TRUE), v_count(2L))
    expect_equivalent(min(x1), v_count(NA_integer_))
    expect_equivalent(max(x1), v_count(NA_integer_))
    expect_equivalent(x1[1:3] + v_count(1L), v_count(c(1L:3L)))
    expect_equivalent(x1[1:3] + 1L, v_count(c(1L:3L)))
    
    expect_is(as_canonical(x1), "integer")
  }
)

test_that(
  "v_count math behaves itself",
  {
    x1 <- v_count(c(0L, 1L, 2L))
    expect_equivalent(range(x1), c(0,2))
    expect_equivalent(median(x1), 1)
  }
)
