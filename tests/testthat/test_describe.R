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
    x1 <- v_rcensored(outcomes = otimes, censors = ctimes, end_time = 15)
    desc <- getDescriptors(x1)
    expect_true(all(c("outcome_info", "censor_info",
                      "time_info", "eair") %in% names(desc)))
  }
)

test_that(
  "describe data.frame",
  {
    df1 <- data.frame(n1 = c(1, 2, 3, 4, 5),
                      n2 = c(6, 7, 8, 9, 10), 
                      n3 = c(NA_integer_, 1, 7, 3, 9))
    
    desc <- describe(df1)
    expect_true(all(c("n1", "n2", "n3") %in% names(desc)))
    expect_error(describe(cbind(df1, cat = c("A", "B", "C", "D", "E"))))
  }
)

test_that(
  "describe continuous stype",
  {
    n1 <- rnorm(n = 5)
    
    cont <- v_continuous(n1)
    desc <- describe(cont)
    expect_true(all(c("sum", "mean", "variance", "median", "iqr", "qtiles") %in% names(desc)))

  }
)