testthat::context("describe functions")

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
  v_event_time(c(5, 6, 10, NA_integer_, 1, NA_integer_, 19), internal_name = "cA"),
  v_event_time(c(4, 1, 15, NA_integer_, NA_integer_, NA_integer_, 21), internal_name = "cB")
)

otimes <- list(
  v_event_time(c(2, 6, 11, 12, NA_integer_, NA_integer_, 25), internal_name = "oA"),
  v_event_time(c(1, NA_integer_, 10, NA_integer_, NA_integer_, NA_integer_, 23), internal_name = "oB")
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
    bin <- v_binary(b1)
    bin_wght1 <- weight(bin, w1)
    bin_wght2 <- weight(bin, w2)
    
    expect_equal(
      get_data_summary(bin_wght1)[["weighted_proportion"]],
      get_data_summary(bin_wght1)[["proportion"]])
    
    expect_equal(
      get_data_summary(bin_wght2)[["weighted_proportion"]],
      sum(w2*b1)/sum(w2)
    )
    
    expect_false(
      get_data_summary(bin_wght2)[["weighted_proportion"]] ==
        get_data_summary(bin_wght2)[["proportion"]]
    )
  }
)
