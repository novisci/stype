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

test_that("context class has get/set methods for all slots", {
  ctxt_slots <- slotNames("context")
  
  purrr::walk(
    .x = ctxt_slots,
    .f = ~ {
      expect_true(methods::hasMethod(sprintf("get_%s", .x), "context"))
      
      if (.x != "purpose"){
        expect_true(
          methods::hasMethod(sprintf("set_%s", .x), c("context", "character"))
        )
      }
      
    }
  )
})

test_that("role predicate functions work on any type", {
  purrr::walk(
    .x = valid_roles,
    .f = function(r){
      f <- sprintf("is_%s", r)
      expect_false(
        do.call(f, args = list(c("x", "z"))),
        info = sprintf("%s should return FALSE for any thing that is not a stype"))
    }
  )
})