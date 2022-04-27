
stype_tester(
  v_type         = "v_continuous",
  canonical_type = "numeric",
  generator      = function() { rnorm(5) },
  error_generators = list(function() { c("A", "B") })
)

test_that(
  "v_continuous operations work",
  {
    x1 <- v_continuous(rnorm(5))
    expect_s3_class(x1 + 5, "v_continuous")
  }
)

test_that(
  "v_continuous works as expected with auto compute off", 
  {
    dat <- rnorm(5)
    x1 <- v_continuous(
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

norm_base <- rnorm(5)
norm_cont <- v_continuous(norm_base)

exp_base <- rexp(5)
exp_cont <- v_continuous(exp_base)
exp_cont_nosum <- v_continuous(exp_base, auto_compute_summary = F)

int_base <- 1L:5L

test_that(
  "v_continuous x v_continuous arithmetic", {
    
    
    # Codomain: v_continuous
    check_arith(
      x = list(norm_cont), 
      y = list(exp_cont), 
      valid = list(
        list(op = `+`, e = v_continuous(norm_base + exp_base)),
        list(op = `-`, e = v_continuous(norm_base - exp_base)),
        list(op = `*`, e = v_continuous(norm_base * exp_base)),
        list(op = `/`, e = v_continuous(norm_base / exp_base)),
        list(op = `^`, e = v_continuous(norm_base^exp_base))
      ),
      invalid = list(
        list(op = `%%`)
      )
    )
    
  }
)

test_that(
  "v_continuous x v_continuous_nonneg arithmetic", {
    
    exp_contnoneg <- v_continuous_nonneg(exp_base)
    
    check_arith(
      x = list(norm_cont), 
      y = list(exp_contnoneg), 
      valid = list(
        list(op = `+`, e = v_continuous(norm_base + exp_base)),
        list(op = `-`, e = v_continuous(norm_base - exp_base)),
        list(op = `*`, e = v_continuous(norm_base * exp_base)),
        list(op = `/`, e = v_continuous(norm_base / exp_base)),
        list(op = `^`, e = v_continuous(norm_base^exp_base))
      ),
      invalid = list(
        list(op = `%%`)
      )
    )
    
  }
)

test_that(
  "v_continuous x double arithmetic", {
    
    check_arith(
      x = list(norm_cont), 
      y = list(exp_base), 
      valid = list(
        list(op = `+`, e = v_continuous(norm_base + exp_base)),
        list(op = `-`, e = v_continuous(norm_base - exp_base)),
        list(op = `*`, e = v_continuous(norm_base * exp_base)),
        list(op = `/`, e = v_continuous(norm_base / exp_base)),
        list(op = `^`, e = v_continuous(norm_base^exp_base))
      ),
      invalid = list(
        list(op = `%%`)
      )
    )
    
    check_arith(
      x = list(5), 
      y = list(norm_cont), 
      valid = list(
        list(op = `+`, e = v_continuous(5 + norm_base)),
        list(op = `-`, e = v_continuous(5 - norm_base)),
        list(op = `*`, e = v_continuous(5 * norm_base)),
        list(op = `/`, e = v_continuous(5 / norm_base)),
        list(op = `^`, e = v_continuous(5^norm_base))
      ),
      invalid = list(
        list(op = `%%`)
      )
    )
    
  }
)

test_that(
  "v_continuous x integer arithmetic", {

    check_arith(
      x = list(norm_cont),
      y = list(int_base),
      valid = list(
        list(op = `+`, e = v_continuous(norm_base + int_base)),
        list(op = `-`, e = v_continuous(norm_base - int_base)),
        list(op = `*`, e = v_continuous(norm_base * int_base)),
        list(op = `/`, e = v_continuous(norm_base / int_base)),
        list(op = `^`, e = v_continuous(norm_base^int_base))
      ),
      invalid = list(
        list(op = `%%`)
      )
    )


    check_arith(
      x = list(5L),
      y = list(norm_cont),
      valid = list(
        list(op = `+`, e = v_continuous(5L + norm_base)),
        list(op = `-`, e = v_continuous(5L - norm_base)),
        list(op = `*`, e = v_continuous(5L * norm_base)),
        list(op = `/`, e = v_continuous(5L / norm_base)),
        list(op = `^`, e = v_continuous(5L ^ norm_base))
      ),
      invalid = list(
        list(op = `%%`)
      )
    )

  }
)

test_that(
  "v_continuous x logical arithmetic", {
    
    check_arith(
      x = list(norm_cont), 
      y = list(TRUE), 
      invalid = list(
        list(op = `%%`),
        list(op = `+`),
        list(op = `-`),
        list(op = `/`),
        list(op = `*`)
      )
    )
  }
)

test_that(
  "v_continuous math operations", {
    
    purrr::walk(
      c(cumsum, cumprod, cummin, cummax),
      function(math_fun){
        expected <- v_continuous(math_fun(exp_base))
        expect_identical(math_fun(exp_cont), expected)
      }
    )
    
    purrr::walk(
      c(abs, exp),
      function(math_fun){
        expected <- v_continuous_nonneg(math_fun(exp_base))
        expect_identical(math_fun(exp_cont), expected)
      }
    )
    
    purrr::walk(
      c(sum, mean, min, max, median, quantile),
      function(math_fun){
        expected <- math_fun(exp_base)
        expect_identical(math_fun(exp_cont), expected)
      }
    )
    
    purrr::walk(
      c(sum, mean, min, max, median, quantile),
      function(math_fun){
        expected <- math_fun(exp_base)
        expect_identical(math_fun(exp_cont_nosum), expected)
      }
    )
    
    expected <- median(exp_base, na.rm = TRUE)
    expect_identical(median(exp_cont, na.rm = TRUE), expected)
    
    expect_error(sin(exp_cont))
    
    # sum, mean for multiple vectors not supported
    check_summary_err(v_continuous(runif(10, -1, 1)))
  }
)

test_that(
  "misc tests for v_continuous", {
    
    ## test footer
    check_footer(x = 1:4, v_continuous, c("Mean", "SD"))
    
    ## test type sum
    expect_equal(type_sum(exp_cont), "cont")
    
    ## test error thrown on multiple inputs to sum
    expect_error(sum(v_continuous(1:4), 1))
    expect_error(sum(v_continuous(1:4), nneg(1:2)))
  }
)

test_that(
  "casting", {
    expect_s3_class(as_continuous(exp_base), "v_continuous")
  }
)
