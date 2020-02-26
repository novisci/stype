testthat::context("Testing v_rcensored")

ctimes <- list(
   v_event_time(c(5, 6, 10, NA_integer_, 1, NA_integer_, 19), internal_name = "cA"),
   v_event_time(c(4, 1, 15, NA_integer_, NA_integer_, NA_integer_, 21), internal_name = "cB")
)

otimes <- list(
  v_event_time(c(2, 6, 11, 12, NA_integer_, NA_integer_, 25), internal_name = "oA"),
  v_event_time(c(1, NA_integer_, 10, NA_integer_, NA_integer_, NA_integer_, 23), internal_name = "oB")
)

stype_tester(
  v_type         = "v_rcensored",
  canonical_type = "list",
  generator      = list(outcomes = otimes, censors = ctimes, end_time = 15),
  error_generators = list(function() { c("A", "B") })
)

test_that("v_rcensored class descriptions update appropriately", {
  x1 <- v_rcensored(outcomes = otimes, censors = ctimes, end_time = 15)
  x2 <- v_rcensored(outcomes = otimes[[1]], end_time = 15)
  
  pcens1  <- get_data_summary(vctrs::field(x1, "censored"), "proportion")
  expect_equivalent(as.numeric(pcens1), 0.2857143)
  
  pcens2  <- get_data_summary(vctrs::field(x1[1:5], "censored"), "proportion")
  expect_equivalent(as.numeric(pcens2), 0.4)
  
  pcens3  <- get_data_summary(vctrs::field(c(x1, x2), "censored"), "proportion")
  expect_equivalent(as.numeric(pcens3), 0.1428571, tolerance = 1e-6)
  
  # TODO: add more thorough tests
  
  # TODO: check that the overal data summaries are working.
  # I know the c(x1, x2) vector is not working
  z <- c(x1, x2)
  expect_equal(get_data_summary(z, "n"), 14L)
  # get_data_summary(x1)
  # get_data_summary(x2)
  # get_data_summary(c(x1, x2))
  
})


test_that("v_rcensored sets levels and labels correctly", {
  
  ctimes1 <- list(
    v_event_time(c(5, 6), internal_name = "cA"),
    v_event_time(c(4, 1), internal_name = "cB")
  )
  ctimes2 <- list(
    v_event_time(c(5, 6)),
    v_event_time(c(4, 1))
  )
  
  otimes <- list(
    v_event_time(c(2, 6), internal_name = "oA",
                 context = stype::context(short_label = "Outcome A"))
  )
  
  x1 <- v_rcensored(outcomes = otimes, censors = ctimes1, end_time = 15)
  x2 <- v_rcensored(outcomes = otimes, censors = ctimes2, end_time = 15)
  
  expect_equal(levels(vctrs::field(x1, "outcome_reason")), c("Outcome A"))
  expect_equal(levels(vctrs::field(x1, "censor_reason")), c("cA", "cB"))
  expect_equal(levels(vctrs::field(x2, "censor_reason")), c("1", "2"))
  
})

test_that("v_rcensored fails as expected", {
  # Different end times
  x1 <- v_rcensored(outcomes = otimes, censors = ctimes, end_time = 15)
  x2 <- v_rcensored(outcomes = otimes[1], censors = ctimes, end_time = 16)
  expect_error(c(x1, x2))
})

test_that("v_rcensored can be cast to a Surv", {
  # Different end times
  x1 <- v_rcensored(outcomes = otimes, censors = ctimes, end_time = 15)

  expect_is(as_Surv(x1), "Surv")
})

test_that("v_rcensored get_data_summary works", {
  x1 <- v_rcensored(outcomes = otimes, censors = ctimes, end_time = 15)
  
  z <- list(
    n = 7L,
    has_missing = FALSE,
    person_time = 55,
    ncensored   = 2,
    nevents     = 3,
    censor_reasons = table(c("cA", "cB", NA, NA, NA, NA, NA), useNA = "always"),
    outcome_reasons = table(c("oA", "oB", "oB", NA, NA, NA, NA), useNA = "always")
  )
  
  expect_equivalent(get_data_summary(x1), z)
})
