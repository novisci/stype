testthat::context("Checks common to all types")

# Exposing the desired_methods
library(vctrs)
library(pillar)

v_types <- names(methods::getClass("stype")@subclasses)

test_that("testing that described types at least have all common methods", {
  desired_methods <- c("obj_print_footer", "format",
                       "vec_ptype_full", "vec_ptype_abbr", "type_sum",
                       "vec_cast", "vec_ptype2", 
                       # "vec_restore", 
                       # commenting this out as it's not clear that v_rcensored needs it
                       "as_canonical")
  purrr::walk(
    .x = v_types,
    .f = ~ {
      mthds <- gsub(paste0("\\.", .x, "$"), "", as.character(utils::methods(class = .x)))
      # browser()
      wh_mthds <- desired_methods %in% mthds
      expect_true(
        all(wh_mthds),
        info = sprintf("%s does not have these methods: %s", .x, 
                       paste(desired_methods[!wh_mthds], collapse = ","))
      )
    }
  )
})

test_that("creating a length 0 v_<type> returns the appropriate type without warnings or errors", {
  purrr::walk(
    .x = v_types,
    .f = ~ {
      expect_s3_class(do.call(.x, args = list()), .x)
    }
  )
})

test_that(
  "type predicate functions work",
  purrr::walk(
    .x = v_types,
    .f = ~ {
      tester <- do.call(.x, args = list())
      expect_true(do.call(gsub("v_", "is_", .x), args = list(x = tester )))
    }
  )
)

test_that(
  "purpose predicate functions work for all types",
  {
    combos <- purrr::cross(list(type = v_types, role = valid_roles))
    purrr::walk(
      .x = combos,
      .f = ~ {
        ctxt   <- context(purpose = purpose(study_role = .x$role))
        tester <- do.call(.x$type, args = list(context = ctxt))
        expect_true(
          is_study_role(tester, .x$role),
          label = sprintf("is_study_role(tester, '%s') failed for %s", .x$role, .x$type))
        expect_true(
          do.call(sprintf("is_%s", .x$role), args = list(tester)),
          label = sprintf("is_%s(tester) failed for %s", .x$role, .x$type))
      }
    )
  }
)
