context("Testing v_count class")

test_that(
  "v_count class behaves itself",
  {
    x1 <- count(c(0L, 1L, 2L, NA_integer_))
    expect_s3_class(x1, "v_count")
    
    expect_equivalent(min(x1, na.rm = TRUE), count(0L))
    expect_equivalent(max(x1, na.rm = TRUE), count(2L))
    expect_equivalent(min(x1), count(NA_integer_))
    expect_equivalent(max(x1), count(NA_integer_))
    
    expect_equivalent(x1[1:3] + count(1L), count(c(1L:3L)))
    
    expect_equivalent(x1[1:3] + 1L, count(c(1L:3L)))
  }
)


test_that(
  "v_count meth behaves itself",
  {
    x1 <- count(c(0L, 1L, 2L))
    expect_equivalent(range(x1), c(0,2))

  }
)
