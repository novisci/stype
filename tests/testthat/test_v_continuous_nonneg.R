
stype_tester(
  v_type = "v_continuous_nonneg",
  canonical_type = "numeric",
  generator = function() {
    abs(rnorm(5))
  },
  error_generators = list(function() {
    c("A", "B")
  })
)

test_that("arithmetic v_continuous_nonneg x v_continuous_nonneg", {
  x1 <- runif(10)
  x2 <- runif(10)
  v1 <- v_continuous_nonneg(x1, context = context(long_label = "irrelevant"))
  v2 <- v_continuous_nonneg(x2, context = context(short_label = "useless"))

  # Codomain: v_continuous_nonneg
  check_arith(
    x = list(v1), 
    y = list(v2), 
    valid = list(
      list(op = `+`, e = v_continuous_nonneg(x1 + x2)),
      list(op = `*`, e = v_continuous_nonneg(x1 * x2)),
      list(op = `/`, e = v_continuous_nonneg(x1 / x2)),
      list(op = `^`, e = v_continuous_nonneg(x1^x2))
    ),
    invalid = list(
      list(op = `%%`)
    )
  )
  
  # Codomain: v_continuous
  check_arith(
    x = list(v_continuous(x1)), 
    y = list(v2), 
    valid = list(
      list(op = `+`, e = v_continuous(x1 + x2)),
      list(op = `*`, e = v_continuous(x1 * x2)),
      list(op = `/`, e = v_continuous(x1 / x2)),
      list(op = `^`, e = v_continuous(x1^x2))
    )
  )
  
  check_arith(
    x = list(v1, x1, v_continuous(x1)), 
    y = list(v2), 
    valid = list(
      list(op = `-`, e = v_continuous(x1 - x2))
    )
  )
  
})

test_that("arithmetic v_continuous_nonneg x v_continuous", {
  x1 <- runif(10)
  x2 <- -runif(10)
  v1 <- v_continuous_nonneg(x1, context = context(long_label = "irrelevant"))
  v2 <- v_continuous(x2, context = context(short_label = "useless"))

  # Codomain: v_continuous_nonneg
  # TODO: add test for v_continuous x v_continuous_nonneg when that is added to
  # v_continuous
  
  check_arith(
    x = list(v1), 
    y = list(v2), 
    valid = list(
      list(op = `+`, e = v_continuous(x1 + x2)),
      list(op = `-`, e = v_continuous(x1 - x2)),
      list(op = `*`, e = v_continuous(x1 * x2)),
      list(op = `/`, e = v_continuous(x1 / x2)),
      list(op = `^`, e = v_continuous_nonneg(x1^x2))
    ),
    invalid = list(
      list(op = `%%`)
    )
  )
  
})

test_that("arithmetic v_continuous_nonneg x double", {
  x1 <- runif(10)
  x2 <- -runif(10)
  v1 <- v_continuous_nonneg(x1, context = context(long_label = "irrelevant"))
  v2 <- x2
  
  # Codomain: v_continuous_nonneg
  # TODO: add test for v_continuous x v_continuous_nonneg when that is added to
  # v_continuous
  
  check_arith(
    x = list(v1), 
    y = list(v2), 
    valid = list(
      list(op = `+`, e = v_continuous(x1 + x2)),
      list(op = `-`, e = v_continuous(x1 - x2)),
      list(op = `*`, e = v_continuous(x1 * x2)),
      list(op = `/`, e = v_continuous(x1 / x2)),
      list(op = `^`, e = v_continuous_nonneg(x1^x2))
    ),
    invalid = list(
      list(op = `%%`)
    )
  )
  
})

test_that("math v_continuous_nonneg", {
  x1 <- runif(10)
  v1 <- v_continuous_nonneg(x1, context = context(long_label = "irrelevant"))

  # Codomain: v_continuous
  # log
  expected <- v_continuous(log(x1))
  expect_identical(log(v1), expected)

  # Codomain: v_continuous_nonneg

  # cumsum
  expected <- v_continuous_nonneg(cumsum(x1))
  expect_identical(cumsum(v1), expected)

  # cumprod
  expected <- v_continuous_nonneg(cumprod(x1))
  expect_identical(cumprod(v1), expected)

  # cummin
  expected <- v_continuous_nonneg(cummin(x1))
  expect_identical(cummin(v1), expected)

  # cummax
  expected <- v_continuous_nonneg(cummax(x1))
  expect_identical(cummax(v1), expected)

  # log1p
  expected <- v_continuous_nonneg(log1p(x1))
  expect_identical(log1p(v1), expected)

  # abs
  expected <- v_continuous_nonneg(abs(x1))
  expect_identical(abs(v1), expected)

  # exp
  expected <- v_continuous_nonneg(exp(x1))
  expect_identical(exp(v1), expected)

  # sqrt
  expected <- v_continuous_nonneg(sqrt(x1))
  expect_identical(sqrt(v1), expected)

  # Summaries

  # sum
  expected <- sum(x1)
  expect_identical(sum(v1), expected)

  # mean
  expected <- mean(x1)
  expect_identical(mean(v1), expected)

  # min
  expected <- min(x1)
  expect_identical(min(v1), expected)

  # max
  expected <- max(x1)
  expect_identical(max(v1), expected)


  # Unsupported ops
  v2 <- v_continuous_nonneg(4)
  expect_error(sin(v2), class = c("stype_invalid_math", "stype_err"))
  check_summary_err(v_continuous_nonneg(runif(10)))
})

test_that(
  "casting/coercion for v_continuous", {
    
    v4 <- v_continuous_nonneg(1:4)

    ## Casting
    expect_error(as_continuous_nonneg(-1:5))
    expect_equal(as_nonneg_continuous(as.double(1:4)), v4)
    #expect_equal(as.double(v4), 1:4)

  }
)

test_that(
  "misc tests for v_continuous", {
    
    v4 <- v_continuous_nonneg(1:4)
    
    ## test footer
    check_footer(x = 1:4, v_continuous_nonneg, c("Mean", "SD"))
    
    ## test type sum
    expect_equal(type_sum(v4), "nneg")
    
    ## Checking logical
    expect_true(is_nonneg(v4))
    expect_false(is_nonneg(v_continuous(1)))
  }
)
