
test_that("standard descriptors work", {
  x <- v_continuous(c(1, NA, 3))
  y <- v_continuous(c(1, NA, 1))
  z <- v_continuous(c(NA, NA, NA))
  
  expect_false(describe(x)$is_constant)
  expect_false(describe(y)$is_constant)
  expect_true(describe(y[-2])$is_constant)
  expect_true(describe(z)$is_constant)
})

test_that(
  "grouped summaries work",
  {
    skip_on_ci() # until smd package installed in r/pkgdev
    x1 <- c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
    g  <- factor(rep(LETTERS[1:2], each = 3))
    desc <- describe(x1, g = g)
    expect_true("smd" %in% names(desc))

  }
)

test_that(
  "grouped AND weighted summaries work",
  {
    skip_on_ci() # until smd package installed in r/pkgdev
    x1 <- c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
    g  <- factor(rep(LETTERS[1:2], each = 3))
    w  <- rep(1:2, each = 3)
    desc <- describe(x1, g = g, w = w)
    expect_true(all(c("smd", "weighted_proportion") %in% names(desc)))


  }
)

test_that(
  "is that a stype?",
  {
    skip_on_ci()
    expect_false(is_stype(1))
  }
)


ctimes <- list(
  v_continuous_nonneg(c(5, 6, 10, Inf, 1, Inf, 19), internal_name = "cA"),
  v_continuous_nonneg(c(4, 1, 15, Inf, Inf, Inf, 21), internal_name = "cB")
)

otimes <- list(
  v_continuous_nonneg(c(2, 6, 11, 12, Inf, Inf, 25), internal_name = "oA"),
  v_continuous_nonneg(c(1, Inf, 10, Inf, Inf, Inf, 23), internal_name = "oB")
)

test_that(
  "v_rcensored describe works",
  {
    skip_on_ci()
    x1 <- v_rcensored(outcomes = otimes, censors = ctimes, end_time = 15)
    desc <- getDescriptors(x1)
    expect_true(all(c("outcome_info", "censor_info",
                      "time_info", "eair") %in% names(desc)))
  }
)


test_that(
  "describe continuous stype",
  {
    skip_on_ci()
    n1 <- rnorm(n = 5)
    
    cont <- v_continuous(n1)
    desc <- describe(cont)
    expect_true(all(c("sum", "mean", "variance", "median", "iqr", "qtiles") %in% names(desc)))

  }
)

test_that(
  "test binary weighting in describe.R",
  {
    skip_on_ci()
    b1 <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)
    w1 <- c(1, 1, 1, 1, 1, 1)
    w2 <- c(1, 2, 3, 4, 5, 6)
    w3 <- v_continuous(w2)
    w4 <- v_continuous_nonneg(w2)
    bin <- v_binary(b1)
    bin_wght1 <- weight(bin, w1)
    bin_wght2 <- weight(bin, w2)
    bin_wght3 <- weight(bin, w3)
    bin_wght4 <- weight(bin, w4)
    
    expect_equal(
      get_data_summary(bin_wght1)[["weighted_proportion"]],
      get_data_summary(bin_wght1)[["proportion"]])
    
    expect_equal(
      get_data_summary(bin_wght2)[["weighted_proportion"]],
      sum(w2*b1)/sum(w2)
    )
    
    expect_equal(
      get_data_summary(bin_wght3)[["weighted_proportion"]],
      sum(w2*b1)/sum(w2)
    )
    
    expect_equal(
      get_data_summary(bin_wght4)[["weighted_proportion"]],
      sum(w2*b1)/sum(w2)
    )
    
    expect_false(
      get_data_summary(bin_wght2)[["weighted_proportion"]] ==
        get_data_summary(bin_wght2)[["proportion"]]
    )
  }
)


test_that(
  "test get_data_summaries",
  {

    dt <-
    list(
      x = v_binary(),
      y = v_continuous(),
      z = v_count()
    )  

    purrr::walk(get_data_summaries(dt),
                ~ expect_is(.x, "data_summary"))
    purrr::walk(get_data_summaries(as.data.frame(dt)), 
                ~ expect_is(.x, "data_summary"))
    purrr::walk(get_data_summaries(tibble::as_tibble(dt)), 
                ~ expect_is(.x, "data_summary"))
  }
)

