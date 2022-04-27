
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
    
    expect_equivalent(min(x1, na.rm = TRUE), 0L)
    expect_equivalent(max(x1, na.rm = TRUE), 2L)
    
    x1 <- v_count(c(0L, 1L, 2L, NA_integer_))
    expect_equivalent(min(x1), NA_integer_)
    expect_equivalent(max(x1), NA_integer_)
    
    expect_equivalent(x1[1:3] + v_count(1L), v_count(c(1L:3L)))
    expect_equivalent(x1[1:3] + 1L, v_count(c(1L:3L)))
  }
)


test_that(
  "v_count works as expected with auto compute off", 
  {
    dat <- 0L:5L
    x1 <- v_count(
      dat,
      auto_compute_summary = FALSE,
      extra_descriptors = list(mean2 = function(x) mean(x*2, na.rm = TRUE)))
    
    # math ops don't update data_summary when auto compute is off
    sx1 <- sum(x1)
    expect_equal(attr(x1, "data_summary"), data_summary())
    mx1 <- mean(x1, na.rm = TRUE)
    expect_equal(mx1, get_data_summary(x1, "mean"))
    expect_equal(attr(x1, "data_summary"), data_summary())
  }
)


test_that(
  "v_count x v_count arithmetic", {
    
    x1 <- 1L:5L
    x2 <- 5L:1L
    v1 <- v_count(x1)
    v2 <- v_count(x2)
    check_arith(
      x = list(v1), 
      y = list(v2), 
      valid = list(
        list(op = `+`, e = v_count(x1 + x2)),
        list(op = `-`, e = x1 - x2)
      ),
      invalid = list(
        list(op = `*`),
        list(op = `/`),
        list(op = `^`),
        list(op = `%%`)
      )
    )
    
  }
)

test_that(
  "v_count x integer arithmetic", {
    
    x1 <- 1L:5L
    x2 <- 6L:10L
    v1 <- v_count(x1)
    v2 <- x2
    check_arith(
      x = list(v1), 
      y = list(v2), 
      valid = list(
        list(op = `+`, e = v_count(x1 + x2)),
        list(op = `-`, e = x1 - x2),
        list(op = `*`, e = v_count(x1*x2)),
        list(op = `^`, e = v_count(x1^x2)),
        list(op = `/`, e = v_continuous(x1/x2))
      ),
      invalid = list(
        list(op = `%%`)
      )
    )
    
  }
)

test_that(
  "integer x v_count arithmetic", {
    
    x1 <- 1L:5L
    x2 <- 6L:10L
    v1 <- x1
    v2 <- v_count(x2)
    check_arith(
      x = list(v1), 
      y = list(v2), 
      valid = list(
        list(op = `+`, e = v_count(x1 + x2)),
        list(op = `-`, e = x1 - x2),
        list(op = `*`, e = v_count(x1*x2)),
        list(op = `^`, e = v_count(x1^x2)),
        list(op = `/`, e = v_continuous(x1/x2))
      ),
      invalid = list(
        list(op = `%%`)
      )
    )
    
  }
)

test_that(
  "v_count math operations", {
    
    x1 <- 1L:5L
    v1 <- v_count(x1)
    purrr::walk(
      c(cumsum, cumprod, cummin, cummax),
      function(math_fun){
        expected <- v_count(math_fun(x1))
        expect_identical(math_fun(v1), expected)
      }
    )
    
    purrr::walk(
      c(abs, exp ,log, log1p, sqrt),
      function(math_fun){
        expected <- v_continuous_nonneg(math_fun(x1))
        expect_identical(math_fun(v1), expected)
      }
    )
    
    purrr::walk(
      c(sum, mean, min, max, median, quantile),
      function(math_fun){
        expected <- math_fun(x1)
        expect_identical(math_fun(v1), expected)
      }
    )

    # sum, mean for multiple vectors not supported
    check_summary_err(v1)
  }
)

test_that(
  "misc tests for v_count", {
    
    ## test footer
    check_footer(x = 1:4, v_count, c("Total", "Mean"))
    
    ## test type sum
    expect_equal(type_sum(v_count(1L:4L)), "cnt")
    
    ## Casting
    expect_error(as_count(0.5:4.5))
    expect_equal(as_count(1L:4L), v_count(1L:4L))

  }
)
    