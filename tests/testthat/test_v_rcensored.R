library(survival)

ctimeA <- v_continuous_nonneg(c(5, 6, 10, Inf, 1, Inf, 19), internal_name = "cA")
ctimeB <- v_continuous_nonneg(c(4, 1, 15, Inf, Inf, Inf, 21), internal_name = "cB")
ctimeC <- v_continuous_nonneg(c(8, 4, 8, Inf, 4, Inf, 19), internal_name = "cC")

otimeA <- v_continuous_nonneg(c(2, 6, 11, 12, Inf, Inf, 25), internal_name = "oA")
otimeB <- v_continuous_nonneg(c(1, Inf, 10, Inf, Inf, Inf, 23), internal_name = "oB")
otimeC <- v_continuous_nonneg(c(3, 4, 8, 12, Inf, Inf, 20), internal_name = "oC")

ctimes <- list(ctimeA, ctimeB)
otimes <- list(otimeA, otimeB)

# This example is taken from the "Multi-state models and competing risks"
# vignette in the survival package.
#
# ‘ptime’: time until progression to a plasma cell malignancy (PCM) or last contact, in months
# ‘pstat’: occurrence of PCM: 0=no, 1=yes
# ‘futime’: time until death or last contact, in months
# ‘death’: occurrence of death: 0=no, 1=yes
mgus2_etime <- with(mgus2, ifelse(pstat == 0, futime, ptime))
mgus2_multiple_event <- with(mgus2, ifelse(pstat == 0, 2 * death, 1))
mgus2_multiple_event <- factor(mgus2_multiple_event, 0:2, labels = c("(censored)", "outcome_pcm", "outcome_death"))

# consider (i) either one of PCM or death to be an event, or (ii) censoring to
# be the event
mgus2_single_event <- ifelse((mgus2$pstat == 1) | (mgus2$death == 1), 1, 0)
mgus2_censor_event <- ifelse(mgus2_single_event == 1, 0, 1)

# Represent the mgus2 data using `v_rcensored` types
mgus2_rcen <- rcen(
  outcomes = list(
    outcome_pcm = nneg(ifelse(mgus2$pstat == 1, mgus2$ptime, Inf), internal_name = "outcome_pcm"),
    outcome_death = nneg(ifelse(mgus2$death == 1, mgus2$futime, Inf), internal_name = "outcome_death")
  ),
  censors = list(
    outcome_pcm = nneg(ifelse(mgus2$pstat == 0, mgus2$ptime, Inf), internal_name = "censor_pcm"),
    outcome_death = nneg(ifelse(mgus2$death == 0, mgus2$futime, Inf), internal_name = "censor_death")
  ),
  internal_name = "mgus2"
)

stype_tester(
  v_type = "v_rcensored",
  canonical_type = "list",
  generator = list(outcomes = otimes, censors = ctimes, end_time = 15),
  error_generators = list(function() {
    c("A", "B")
  })
)

test_that("v_rcensored class descriptions update appropriately", {
  x1 <- v_rcensored(outcomes = otimes, censors = ctimes, end_time = 15)
  x2 <- v_rcensored(outcomes = otimes[[1]], end_time = 15)

  pcens1 <- get_data_summary(vctrs::field(x1, "censored"), "proportion")
  expect_equivalent(as.numeric(pcens1), 0.2857143)

  pcens2 <- get_data_summary(vctrs::field(x1[1:5], "censored"), "proportion")
  expect_equivalent(as.numeric(pcens2), 0.4)

  pcens3 <- get_data_summary(vctrs::field(c(x1, x2), "censored"), "proportion")
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

test_that("predicate function for v_rcensored types", {
  expect_false(is_rcensored(22))
  expect_true(is_rcensored(v_rcensored(outcomes = otimes, censors = ctimes, end_time = 15)))
})

test_that("v_rcensored sets levels and labels correctly", {
  ctimes1 <- list(
    v_continuous_nonneg(c(5, 6), internal_name = "cA"),
    v_continuous_nonneg(c(4, 1), internal_name = "cB")
  )
  ctimes2 <- list(
    v_continuous_nonneg(c(5, 6)),
    v_continuous_nonneg(c(4, 1))
  )

  otimes <- list(
    v_continuous_nonneg(c(2, 6),
      internal_name = "oA",
      context = stype::context(short_label = "Outcome A")
    )
  )

  x1 <- v_rcensored(outcomes = otimes, censors = ctimes1, end_time = 15)
  x2 <- v_rcensored(outcomes = otimes, censors = ctimes2, end_time = 15)

  expect_equal(levels(vctrs::field(x1, "outcome_reason")), c("Outcome A"))
  expect_equal(levels(vctrs::field(x1, "censor_reason")), c("cA", "cB"))
  expect_equal(levels(vctrs::field(x2, "censor_reason")), c("1", "2"))

  r1 <- v_rcensored(
    list(o1 = v_continuous_nonneg(), o2 = v_continuous_nonneg()),
    list(c1 = v_continuous_nonneg(), c2 = v_continuous_nonneg())
  )

  expect_equal(levels(get_outcome_reason(r1)), c("o1", "o2"))
  expect_equal(levels(get_censor_reason(r1)), c("c1", "c2"))


  r2 <- v_rcensored(
    list(
      v_continuous_nonneg(internal_name = "o1"),
      v_continuous_nonneg(internal_name = "o2")
    ),
    list(c1 = v_continuous_nonneg(), c2 = v_continuous_nonneg())
  )

  expect_equal(levels(get_outcome_reason(r2)), c("o1", "o2"))
  expect_equal(levels(get_censor_reason(r2)), c("c1", "c2"))

  # inputs must be consistent in how they are named, else defaults to integer levels
  r3 <- v_rcensored(
    list(
      v_continuous_nonneg(internal_name = "o1"),
      v_continuous_nonneg(context = context(short_label = "o2"))
    ),
    list(c1 = v_continuous_nonneg(), c2 = v_continuous_nonneg())
  )
  expect_equal(levels(get_outcome_reason(r3)), c("1", "2"))
  expect_length(labels(get_outcome_reason(r3)), 0L)
  expect_equal(levels(get_censor_reason(r3)), c("c1", "c2"))

  r4 <- v_rcensored(list(v_continuous_nonneg(internal_name = "o1"),
    o2 = v_continuous_nonneg()
  ))

  expect_equal(levels(get_outcome_reason(r4)), c("1", "2"))

  ## Labels and Names
  r5 <- v_rcensored(
    list(
      o1 = v_continuous_nonneg(context = context(short_label = "out1")),
      o2 = v_continuous_nonneg(context = context(short_label = "out2"))
    ),
    list(
      c1 = v_continuous_nonneg(context = context(short_label = "cens1")),
      c2 = v_continuous_nonneg(context = context(short_label = "cens2"))
    )
  )

  expect_equal(levels(get_outcome_reason(r5)), c("out1", "out2"))
  expect_equal(levels(get_censor_reason(r5)), c("cens1", "cens2"))
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
  expect_is(as_Surv(x1, censor_as_event = TRUE), "Surv")
})

test_that("v_rcensored cast to a Surv is equivalent to Surv", {

  # single event data
  actual <- as_Surv(mgus2_rcen)
  expected <- Surv(mgus2_etime, mgus2_single_event)
  expect_identical(actual, expected)

  # single event data with censoring considered as an event
  actual <- as_Surv(mgus2_rcen, censor_as_event = TRUE)
  expected <- Surv(mgus2_etime, mgus2_censor_event)
  expect_identical(actual, expected)

  # multiple event data
  actual <- as_Surv(mgus2_rcen, multiple_endpoint = TRUE)
  expected <- Surv(mgus2_etime, mgus2_multiple_event)
  expect_identical(actual, expected)

  # multiple event data with censoring considered as an event
  expect_error(as_Surv(mgus2_rcen, censor_as_event = TRUE, multiple_endpoint = TRUE))
})

test_that("v_rcensored get_data_summary works", {
  x1 <- v_rcensored(outcomes = otimes, censors = ctimes, end_time = 15)

  z <- list(
    n = 7L,
    has_missing = FALSE,
    n_nonmissing = 7,
    n_missing = 0,
    proportion_missing = 0,
    is_constant = FALSE,
    person_time = 55,
    n_events = 3,
    outcome_reasons = table(c("oA", "oB", "oB", NA, NA, NA, NA), useNA = "always"),
    n_censored = 2,
    censor_reasons = table(c("cA", "cB", NA, NA, NA, NA, NA), useNA = "always"),
    eair = 0.54545454,
    eair_variance = 0.006923789
  )

  expect_equivalent(get_data_summary(x1), z, tolerance = 8)
})


test_that("v_rcensored extra_descriptors works", {
  x1 <- v_rcensored(
    outcomes = otimes, censors = ctimes, end_time = 15,
    extra_descriptors = list(length = function(x) length(x))
  )
  x2 <- x1[1:5]
  expect_true("length" %in% names(get_data_summary(x2)))
})

test_that("row binding works for v_rcensored", {
  x1 <- v_rcensored(outcomes = otimes, censors = ctimes, end_time = 15)
  x2 <- v_rcensored(outcomes = otimes, censors = ctimes, end_time = 15)
  x3 <- v_rcensored(outcomes = otimes, censors = ctimes, end_time = 15)

  z1 <- tibble::tibble(x = x1, y = x2)
  z2 <- tibble::tibble(x = x2, y = x3)
  z3 <- tibble::tibble(x = x3, y = x1)

  expect_is(vctrs::vec_rbind(z1, z2), "data.frame")
  expect_is(vctrs::vec_rbind(z1, z3), "data.frame")
  expect_is(vctrs::vec_rbind(z2, z3), "data.frame")
  expect_is(vctrs::vec_rbind(z1, z2, z3), "data.frame")

  # TODO: once dplyr with vctrs the hood is stable add tests for:
  # bind_rows(z1, z2)
  # bind_rows(z1, z2, z3)

  # TODO: this doesn't work (I don't know how to begin to fix)
  # rbind(z1, z2)
  # rbind(z1, z2, z3)
})


test_that("v_rcensored getters work", {
  x1 <- v_rcensored(outcomes = otimes, censors = ctimes, end_time = 15)

  expect_is(get_time(x1), "v_continuous_nonneg")
  expect_is(get_censored(x1), "v_binary")
  expect_is(get_outcome(x1), "v_binary")
  expect_is(get_censor_reason(x1), "v_nominal")
  expect_is(get_outcome_reason(x1), "v_nominal")
})

test_that("sort of v_rcensored works", {
  x1 <- v_rcensored(outcomes = otimes, censors = ctimes, end_time = 15)

  x2 <- sort(x1)

  expect_equal(get_context(x1), get_context(x2))
  expect_equal(get_internal_name(x1), get_internal_name(x2))
  expect_equal(get_data_summary(x1), get_data_summary(x2))

  expect_equal(sort(get_time(x1)), get_time(x2))
  expect_equal(
    as_canonical(get_outcome(x2)),
    c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)
  )
  expect_equal(
    as_canonical(get_censored(x2)),
    c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("pmin_v_rcensored equivalent values", {
  x1 <- v_rcensored(outcomes = list(oA = otimeA), censors = list(cA = ctimeA))
  x2 <- v_rcensored(outcomes = list(oB = otimeB), censors = list(cB = ctimeB))
  x3 <- v_rcensored(outcomes = list(oC = otimeC), censors = list(cC = ctimeC))

  # one input
  actual <- pmin_v_rcensored(x1)
  expected <- v_rcensored(
    outcomes = list(oA = otimeA),
    censors  = list(cA = ctimeA),
    end_time = Inf
  )
  expect_equal(actual, expected)

  # three inputs
  actual <- pmin_v_rcensored(x1, x2, x3)
  expected <- v_rcensored(
    outcomes = list(oA = otimeA, oB = otimeB, oC = otimeC),
    censors  = list(cA = ctimeA, cB = ctimeB, cC = ctimeC),
    end_time = Inf
  )
  expect_equal(actual, expected)

  # three inputs -- changed order
  actual <- pmin_v_rcensored(x3, x1, x2)
  expected <- v_rcensored(
    outcomes = list(oC = otimeC, oA = otimeA, oB = otimeB),
    censors  = list(cC = ctimeC, cA = ctimeA, cB = ctimeB),
    end_time = Inf
  )
  expect_equal(actual, expected)

  # sequential combinations
  actual <- pmin_v_rcensored(pmin_v_rcensored(x1, x2), x3)
  expected <- v_rcensored(
    outcomes = list(oA = otimeA, oB = otimeB, oC = otimeC),
    censors  = list(cA = ctimeA, cB = ctimeB, cC = ctimeC),
    end_time = Inf
  )
  expect_equal(actual, expected)
})

test_that("pmin_v_rcensored end time selection", {
  a_15 <- v_rcensored(outcomes = list(oA = otimeA), censors = list(cA = ctimeA), end_time = 15)
  b_15 <- v_rcensored(outcomes = list(oB = otimeB), censors = list(cB = ctimeB), end_time = 15)
  c_15 <- v_rcensored(outcomes = list(oC = otimeC), censors = list(cC = ctimeC), end_time = 15)

  # end times all equal
  actual_strict <- pmin_v_rcensored(a_15, b_15, c_15, new_end_time = "strict")
  actual_min <- pmin_v_rcensored(a_15, b_15, c_15, new_end_time = "strict")
  actual_dbl15 <- pmin_v_rcensored(a_15, b_15, c_15, new_end_time = 15)
  expected_15 <- v_rcensored(
    outcomes = list(oA = otimeA, oB = otimeB, oC = otimeC),
    censors  = list(cA = ctimeA, cB = ctimeB, cC = ctimeC),
    end_time = 15
  )
  expect_equal(actual_strict, expected_15)
  expect_equal(actual_min, expected_15)
  expect_equal(actual_dbl15, expected_15)

  a_12 <- v_rcensored(outcomes = list(oA = otimeA), censors = list(cA = ctimeA), end_time = 12)
  b_18 <- v_rcensored(outcomes = list(oB = otimeB), censors = list(cB = ctimeB), end_time = 18)
  c_22 <- v_rcensored(outcomes = list(oC = otimeC), censors = list(cC = ctimeC), end_time = 22)

  # end times differ
  actual_min <- pmin_v_rcensored(a_12, b_18, c_22, new_end_time = "min")
  actual_dbl10 <- pmin_v_rcensored(a_12, b_18, c_22, new_end_time = 10)
  actual_dbl12 <- pmin_v_rcensored(a_12, b_18, c_22, new_end_time = 12)
  expected_10 <- v_rcensored(
    outcomes = list(oA = otimeA, oB = otimeB, oC = otimeC),
    censors  = list(cA = ctimeA, cB = ctimeB, cC = ctimeC),
    end_time = 10
  )
  expected_12 <- v_rcensored(
    outcomes = list(oA = otimeA, oB = otimeB, oC = otimeC),
    censors  = list(cA = ctimeA, cB = ctimeB, cC = ctimeC),
    end_time = 12
  )
  expect_error(pmin_v_rcensored(a_12, b_18, c_22, new_end_time = "strict"))
  expect_equal(actual_min, expected_12)
  expect_equal(actual_dbl10, expected_10)
  expect_equal(actual_dbl12, expected_12)
  expect_error(pmin_v_rcensored(a_12, b_18, c_22, new_end_time = 13))
})


test_that("invalid v_rcensored inputs are properly handled with decent error messages", {
  expect_error(
    v_rcensored(outcomes = NULL),
    regexp = "at least one outcome"
  )

  expect_error(
    v_rcensored(censors = list(y = v_continuous_nonneg(5))),
    regexp = "must have the same length"
  )

  expect_error(
    v_rcensored(
      outcomes = v_continuous_nonneg(c(2, 5)),
      censors = list(v_continuous_nonneg(2))
    ),
    regexp = "must have the same length"
  )

  expect_error(
    v_rcensored(
      outcomes = v_continuous_nonneg(c(2, 5)),
      censors = list(
        v_continuous_nonneg(2, 3),
        v_continuous_nonneg(2, 3, 5)
      )
    ),
    regexp = "must have the same length"
  )

  expect_error(
    v_rcensored(
      outcomes = list(y = v_continuous_nonneg(10)),
      censors = list(y = 5)
    ),
    regexp = "v_continuous_nonneg"
  )

  expect_error(
    v_rcensored(outcomes = list(y = 10)),
    regexp = "v_continuous_nonneg"
  )

  expect_error(
    v_rcensored(outcomes = v_continuous_nonneg(1), v_continuous_nonneg(NA)),
    regexp = "NA"
  )
})

test_that(
  "misc tests for v_rcensored",
  {

    ## test footer
    expect_output(print(v_rcensored(otimes, ctimes)))

    ## small name?
    expect_equal(type_sum(v_rcensored(otimes, ctimes)), "rcen")

    ## auto compute summary = F works
    sum_on <- v_rcensored(outcomes = otimes, censors = ctimes)
    sum_off <- v_rcensored(outcomes = otimes, censors = ctimes, auto_compute_summary = F)

    # expect_equal(sum_on, sum_off)
  }
)

test_that(
  "casting intermediate vectors works to construct right censored",
  {
    test_rcens_cast <- function(fn) {
      coerce_cA <- fn(c(5, 6, 10, Inf, 1, Inf, 19))
      coerce_cB <- fn(c(4, 1, 15, Inf, Inf, Inf, 21))

      coerce_oA <- fn(c(2, 6, 11, 12, Inf, Inf, 25))
      coerce_oB <- fn(c(1, Inf, 10, Inf, Inf, Inf, 23))

      c_coerce <- list(coerce_cA, coerce_cB)
      o_coerce <- list(coerce_oA, coerce_oB)

      v_rcensored(outcomes = o_coerce, censor = c_coerce)
    }

    cast_rcens <- test_rcens_cast(as_nonneg_continuous)
    construct_rcens <- test_rcens_cast(v_continuous_nonneg)

    expect_equal(cast_rcens, construct_rcens)
  }
)
