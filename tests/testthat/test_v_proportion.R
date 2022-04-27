
stype_tester(
  v_type = "v_proportion",
  canonical_type = "numeric",
  generator = function() {
    runif(5)
  },
  error_generators = list(function() {
    c("A", "B")
  })
)

test_that("arithmetic v_proportion x v_proportion", {
  x1 <- runif(10)
  x2 <- runif(10)
  v1 <- v_proportion(x1, context = context(long_label = "irrelevant"))
  v2 <- v_proportion(x2, context = context(short_label = "useless"))

  check_arith(
    x = list(v1), 
    y = list(v2), 
    valid = list(
      list(op = `+`, e = v_continuous_nonneg(x1 + x2)),
      list(op = `-`, e = v_continuous(x1 - x2)),
      list(op = `*`, e = v_proportion(x1 * x2)),
      list(op = `/`, e = v_continuous_nonneg(x1 / x2)),
      list(op = `^`, e = v_proportion(x1^x2))
    ),
    invalid = list(
      list(op = `%%`)
    )
  )

  check_arith(
    x = list(x1), 
    y = list(v2), 
    valid = list(
      list(op = `+`, e = v_continuous(x1 + x2)),
      list(op = `-`, e = v_continuous(x1 - x2)),
      list(op = `*`, e = v_continuous(x1 * x2)),
      list(op = `/`, e = v_continuous(x1 / x2)),
      list(op = `^`, e = v_continuous(x1^x2))
    ),
    invalid = list(
      list(op = `%%`)
    )
  )
})

test_that("arithmetic v_proportion x v_continuous", {
  x1 <- runif(10)
  x2 <- rnorm(10)
  v1 <- v_proportion(x1, context = context(long_label = "irrelevant"))
  v2 <- v_continuous(x2, context = context(short_label = "useless"))
  
  # TODO: add test for v_continuous x v_proportion when that is added to
  
  check_arith(
    x = list(v1), 
    y = list(v2), 
    valid = list(
      list(op = `+`, e = v_continuous(x1 + x2)),
      list(op = `-`, e = v_continuous(x1 - x2)),
      list(op = `*`, e = v_continuous(x1 * x2)),
      list(op = `/`, e = v_continuous(x1 / x2)),
      list(op = `^`, e = v_continuous(x1^x2))
    ),
    invalid = list(
      list(op = `%%`)
    )
  )
  
})

test_that("math v_proportion", {
  x1 <- runif(10)
  v1 <- v_proportion(x1, context = context(long_label = "irrelevant"))

  # Codomain: v_continuous
  # log
  expected <- v_continuous(log(x1))
  expect_identical(log(v1), expected)

  # log1p
  expected <- v_continuous(log1p(x1))
  expect_identical(log1p(v1), expected)
  
  # exp
  expected <- v_continuous(exp(x1))
  expect_identical(exp(v1), expected)
  
  # Codomain: v_continuous_nonneg

  # cumsum
  expected <- v_continuous_nonneg(cumsum(x1))
  expect_identical(cumsum(v1), expected)

  # cummin
  expected <- v_proportion(cummin(x1))
  expect_identical(cummin(v1), expected)

  # cummax
  expected <- v_proportion(cummax(x1))
  expect_identical(cummax(v1), expected)

  # Codomain: v_proportion
  
  # cumprod
  expected <- v_proportion(cumprod(x1))
  expect_identical(cumprod(v1), expected)
  
  # abs
  expected <- v_proportion(abs(x1))
  expect_identical(abs(v1), expected)



  # sqrt
  expected <- v_proportion(sqrt(x1))
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
  v2 <- v_proportion(0.5)
  expect_error(sin(v2), class = c("stype_invalid_math", "stype_err"))
  check_summary_err(v2)
})

test_that(
  "misc tests for v_proportion", {
    
    p1 <- v_proportion(1/(1:4))
    
    expect_true(is_prop(p1))
    ## test footer
    check_footer(x = 1/(1:4), v_proportion, c("Mean", "SD"))
    
    ## test type sum
    expect_equal(type_sum(p1), "prop")
    
  }
)

test_that(
  "v_proportion casting and coercion", {
    p1 <- v_proportion(1/(1:4))
    c1 <- as_continuous(-0.5:3.5)
    nc1 <- v_continuous_nonneg(0.5:4.5)
    expect_equal(as_proportion(1/(1:4)), p1)
    
    vec_ptype2(p1, c1)

  }
)

