testthat::context("testing purpose object class")

test_that("purpose class and predicate functions work as hoped", {
  
  purrr::walk(
    .x = valid_roles,
    .f = ~ {
      tester <- purpose(study_role = .x)
      expect_s4_class(tester, "purpose")
      expect_true(is_study_role(tester, .x))
      expect_true(do.call(sprintf("is_%s", .x), args = list(tester)))
    }
  )

})