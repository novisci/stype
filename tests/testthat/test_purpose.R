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

test_that("tag predicates work", {
  tester1 <- purpose(study_role = "outcome", 
                    tags = c("primary", "jdkfal"))
  
  tester2 <- purpose(study_role = "outcome")
  
  expect_true(is_tagged(tester1)) 
  expect_false(is_not_tagged(tester1)) 
  
  expect_false(is_tagged(tester2)) 
  expect_true(is_not_tagged(tester2))
  
  expect_true(has_tag(tester1, "primary"))
  expect_false(has_tag(tester2, "primary"))
   
  expect_true(has_tag(tester1, "primary"))
  expect_false(has_tag(tester1, "not"))
  expect_error(has_tag(tester1, c("A", "B")))
  
  expect_true(has_any_tags(tester1, c("primary", "B")))
  expect_false(has_any_tags(tester1, c("A", "B")))
  expect_true(has_all_tags(tester1, c("primary", "jdkfal")))
  expect_false(has_all_tags(tester1, c("primary", "jdkfal", "A")))
  expect_true(has_all_tags(tester1, c("primary")))
  expect_true(has_only_tags(tester1, c("primary", "jdkfal")))
  expect_true(has_only_tags(tester1, c("jdkfal", "primary")))
  expect_false(has_only_tags(tester1, c("primary", "jdkfal", "A")))
  expect_false(has_only_tags(tester1, c("primary")))
  
  expect_false(has_any_tags(tester2, "primary")) 
  expect_false(has_all_tags(tester2, "primary")) 
  expect_false(has_only_tags(tester2, "primary")) 
})