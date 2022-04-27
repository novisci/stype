test_that("an analysis can be created", {
  expect_is(analysis(), "tbl_analysis")
})

test_that("an analysis exists", {
  expect_true(is_analysis(analysis()))
})

test_that("an analysis can be de-created", {
  expect_is(as_canonical(analysis()), "data.frame")
})
