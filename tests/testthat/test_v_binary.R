
stype_tester(
  v_type         = "v_binary",
  canonical_type = "logical",
  generator      = function() { c(TRUE, FALSE, TRUE, FALSE, NA) },
  error_generators = list(function() { c("A", "B") })
)

test_that(
  "v_binary operations work and fail appropriately",
  {
    x1 <- v_binary(c(TRUE, FALSE, TRUE, FALSE, NA))
    expect_s3_class(!x1, "v_binary")
    expect_equivalent(vctrs::vec_data(!x1), c(FALSE, TRUE, FALSE, TRUE, NA))
    expect_equivalent(view(!x1, auto_compute_summary_l), 
                      view(x1,  auto_compute_summary_l))
    expect_equivalent(view(!x1, extra_descriptors_l), 
                      view(x1,  extra_descriptors_l))
    
    x2 <- v_binary(c(TRUE, FALSE, TRUE, FALSE, NA),
                   auto_compute_summary = FALSE,
                   extra_descriptors = list(not_mean = function(x) mean(!x, na.rm = TRUE)))
    expect_s3_class(!x2, "v_binary")
    expect_equivalent(vctrs::vec_data(!x2), c(FALSE, TRUE, FALSE, TRUE, NA))
    expect_equal(view(!x2, auto_compute_summary_l), 
                 view(x2,  auto_compute_summary_l))
    expect_equal(view(!x2, extra_descriptors_l), 
                 view(x2,  extra_descriptors_l))
    # sum doesn't update data_summary when auto compute is off
    sx2 <- sum(x2)
    expect_equal(attr(x2, "data_summary"), data_summary())
    mx2 <- mean(x2, na.rm = TRUE)
    expect_equal(mx2, get_data_summary(x2, "proportion"))
    expect_equal(attr(x2, "data_summary"), data_summary())
    
    
    expect_equal(any(x1, na.rm = TRUE), TRUE)
    expect_equal(all(x1[c(1, 3)], na.rm = TRUE), TRUE)
    
    expect_equal(sum(x1[1:4]), 2)

    # sum, mean for multiple vectors not supported
    check_summary_err(x1)
    
    expect_equal(x1 | x1, c(TRUE, FALSE, TRUE, FALSE, NA))
    expect_equal(c(TRUE, FALSE, TRUE, FALSE, NA) | x1, 
                 c(TRUE, FALSE, TRUE, FALSE, NA))
    expect_equal(x1 | c(TRUE, FALSE, TRUE, FALSE, NA), 
                 c(TRUE, FALSE, TRUE, FALSE, NA))
    expect_error(x1 | x1[1:2])
    expect_equal(x1 + x1, v_binary(c(TRUE, FALSE, TRUE, FALSE, NA)))
    expect_equal(x1 & x1, c(TRUE, FALSE, TRUE, FALSE, NA))
    expect_equal(x1 & c(TRUE, FALSE, TRUE, FALSE, NA),
                 c(TRUE, FALSE, TRUE, FALSE, NA))
    expect_equal(c(TRUE, FALSE, TRUE, FALSE, NA) & x1,
                 c(TRUE, FALSE, TRUE, FALSE, NA))
    expect_error(x1 & x1[1:2])
    expect_equal(x1 * x1, v_binary(c(TRUE, FALSE, TRUE, FALSE, NA)))
    
  }
)

test_that(
  "v_binary math operations", {
    
    x <- rbinom(10, 1, prob = 0.5)
    b1 <- as_binary(x)
    purrr::walk(
      c(cumsum),
      function(math_fun){
        expected <- v_count(math_fun(x))
        expect_identical(math_fun(b1), expected)
      }
    )
    
    purrr::walk(
      c(cumprod, cummin, cummax),
      function(math_fun){
        expected <- v_binary(math_fun(x))
        expect_identical(math_fun(b1), expected)
      }
    )
    
    expect_identical(mean(b1), mean(x))
    expect_identical(sum(x), sum(b1)) 
    
    expect_error(sin(b1))
  }
)

test_that("arithmetic v_binary x double", {
  v1 <- v_binary(rbinom(10, 1, prob = 0.5))
  v2 <- rnorm(10)
  
  check_arith(
    x = list(v1), 
    y = list(v2), 
    valid = list(
      list(op = `*`, e = v2 * as.integer(as_canonical(v1))),
      list(op = `^`, e = v1^v2)
    ),
    invalid = list(
      list(op = `-`),
      list(op = `/`),
      list(op = `+`)
    )
  )
  
})

test_that("arithmetic v_binary x integer", {
  v1 <- v_binary(rbinom(10, 1, prob = 0.5))
  v2 <- 1L:10L
  
  # Codomain: v_continuous_nonneg
  # TODO: add test for v_continuous x v_continuous_nonneg when that is added to
  # v_continuous
  
  check_arith(
    x = list(v1), 
    y = list(v2), 
    valid = list(
      list(op = `*`, e = v2 * as.integer(as_canonical(v1))),
      list(op = `^`, e = v1^v2)
    ),
    invalid = list(
      list(op = `-`),
      list(op = `/`),
      list(op = `+`)
    )
  )
  
})

test_that("arithmetic v_continuous x v_binary", {
  x1 <- 1:10
  x2 <- rbinom(10, 1, prob = 0.5)
  v1 <- v_continuous(x1)
  v2 <- v_binary(x2)
  
  
  # Codomain: v_continuous_nonneg
  # TODO: add test for v_continuous x v_continuous_nonneg when that is added to
  # v_continuous
  
  check_arith(
    x = list(v1), 
    y = list(v2), 
    valid = list(
      list(op = `*`, e = v_continuous(x1*x2)),
      list(op = `^`, e = v_continuous(x1^x2))
    ),
    invalid = list(
      list(op = `-`),
      list(op = `/`),
      list(op = `+`)
    )
  )
  
})

test_that("arithmetic v_count x v_binary", {
  x1 <- 1:10
  x2 <- rbinom(10, 1, prob = 0.5)
  v1 <- v_count(x1)
  v2 <- v_binary(x2)
  
  
  # Codomain: v_continuous_nonneg
  # TODO: add test for v_continuous x v_continuous_nonneg when that is added to
  # v_continuous
  
  check_arith(
    x = list(v1), 
    y = list(v2), 
    valid = list(
      list(op = `*`, e = v_count(x1*x2)),
      list(op = `^`, e = v_count(x1^x2))
    ),
    invalid = list(
      list(op = `-`),
      list(op = `/`),
      list(op = `+`)
    )
  )
  
})

test_that("arithmetic v_binary x v_binary", {
  x1 <- as.logical(rbinom(10, 1, prob = 0.5))
  x2 <- as.logical(rbinom(10, 1, prob = 0.5))
  v1 <- v_binary(x1)
  v2 <- v_binary(x2)
  
  
  # Codomain: v_continuous_nonneg
  # TODO: add test for v_continuous x v_continuous_nonneg when that is added to
  # v_continuous
  
  check_arith(
    x = list(v1), 
    y = list(v2), 
    valid = list(
      list(op = `*`, e = v_binary(x1 & x2)),
      list(op = `+`, e = v_binary(x1|x2))
    ),
    invalid = list(
      list(op = `-`),
      list(op = `/`)
    )
  )
  
})

test_that(
  "misc tests for v_proportion", {
    
    x <- rbinom(10, 1, prob = 0.5)
    b1 <- as_binary(x)
    
    expect_true(is_binary(b1))
    ## test footer
    check_footer(x = x, v_binary, c("Proportion"))
    
    ## test type sum
    expect_equal(type_sum(b1), "bnry")
    
  }
)

test_that(
  "v_binary casting and coercion", {
    b1 <- v_binary(T)
    b2 <- as_binary(T)

    expect_equal(b1, b2)
    
    expect_s3_class(c(b1, T), "v_binary")
  }
)


