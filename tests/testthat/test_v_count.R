testthat::context("Testing v_count class")

stype_tester(
  v_type         = "v_count",
  canonical_type = "integer",
  generator      = function() { c(0L, 1L, 2L, NA_integer_) },
  error_generators = list(function() { c("A", "B") })
)

test_that(
  "v_count math behaves itself",
  {
    x1 <- v_count(c(0L, 1L, 2L))
    expect_equivalent(range(x1), c(0,2))
    expect_equivalent(median(x1), 1)
    
    expect_equivalent(min(x1, na.rm = TRUE), v_count(0L))
    expect_equivalent(max(x1, na.rm = TRUE), v_count(2L))
    
    x1 <- v_count(c(0L, 1L, 2L, NA_integer_))
    expect_equivalent(min(x1), v_count(NA_integer_))
    expect_equivalent(max(x1), v_count(NA_integer_))
    
    expect_equivalent(x1[1:3] + v_count(1L), v_count(c(1L:3L)))
    expect_equivalent(x1[1:3] + 1L, v_count(c(1L:3L)))
  }
)
