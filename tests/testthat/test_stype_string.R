
test_that("stype_str methods", {

  # names of the list are the results we expect from `stype_str`
  instantiated_vec_list <- list(
    "binary"                 = v_binary(c(TRUE, FALSE, TRUE, FALSE, NA)),
    "character"              = v_character(c("This", "is", "some", "vector")),
    "continuous"             = v_continuous(c(-3, -1, 1, 3)),
    "continuous nonnegative" = v_continuous_nonneg(abs(c(-3, -1, 1, 3))),
    "count"                  = v_count(c(0L, 1L, 2L)),
    "time-to-event"          = v_event_time(c(1, 2, 3)),
    "nominal"                = v_nominal(factor(letters)),
    "ordered"                = v_ordered(factor(letters))
  )
  actual <- unname(purrr::map_chr(instantiated_vec_list, stype_str))
  expected <- names(instantiated_vec_list)

  expect_identical(actual, expected)
})

test_that("stype_str invalid input", {
  expect_error(stype_str(1L:5L), "not a stype object")
})
