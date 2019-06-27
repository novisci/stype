testthat::context("Checks common to all types")

# Exposing the desired_methods
library(vctrs)
library(pillar)

v_types <- names(methods::getClass("described")@subclasses)

test_that("testing that described types at least have all common methods", {
  desired_methods <- c("obj_print_footer", "format",
                       "vec_ptype_full", "vec_ptype_abbr", "type_sum",
                       "vec_cast", "vec_type2", "vec_restore")
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
  