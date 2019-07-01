testthat::context("testing context object class")

test_that("context class and predicate functions work as hoped", {
  
  purrr::walk(
    .x = valid_roles,
    .f = ~ {
      tester <- context(purpose = purpose(study_role = .x))
      expect_s4_class(tester, "context")
      expect_true(is_study_role(tester, .x))
      expect_true(do.call(sprintf("is_%s", .x), args = list(tester)))
    }
  )
  
})