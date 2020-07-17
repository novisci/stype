
testthat::test_that("all context slots have a lens", {
  cslots     <- slotNames(context())
  stype_lens <- ls("package:stype")[grepl("_l$", ls("package:stype"))]
  
  purrr::walk(
    .x = cslots,
    .f = ~ { 
      expect_true(
        paste0(.x, "_l") %in% stype_lens,
        info = sprintf("%s does not have a lens", .x)
      )
    }
  )
  
})

testthat::test_that("context lens view and set", {
  x1 <- v_binary(context = context(
    derivation = "some derivation",
    long_label = "some long label",
    short_label = "some short label",
    description = "some description",
    purpose    = purpose(study_role = "outcome"),
    security_type = "some security type"
  ))
  
  purrr::walk2(
    .x = list(derivation_l, 
              short_label_l,
              long_label_l,
              description_l,
              security_type_l),
    .y = c("some derivation",
           "some short label",
           "some long label",
           "some description",
           "some security type"),
    .f = ~{
      expect_equal(view(x1, .x), .y)
      new <- gsub("some", "another", .y)
      x2 <- set(x1, derivation_l, new)
      expect_equal(view(x2, derivation_l), new) 
    }
  )
  
  # test the purpose lenses
  expect_equal(view(x1, purpose_l), purpose(study_role = "outcome"))
  x2 <- set(x1, purpose_l, purpose(study_role = "covariate"))
  expect_equal(view(x2, purpose_l), purpose(study_role = "covariate")) 
  expect_equal(view(x2, study_role_l), "covariate")
  x2 <- set(x1, study_role_l, "outcome")
  expect_equal(view(x2, study_role_l), "outcome")
  
  
})