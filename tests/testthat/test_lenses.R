
stype_lenses <- ls("package:stype")[grepl("_l$", ls("package:stype"))]

testthat::test_that("all context slots have a lens", {
  cslots <- slotNames(context())

  purrr::walk(
    .x = cslots,
    .f = ~ {
      expect_true(
        paste0(.x, "_l") %in% stype_lenses,
        info = sprintf("%s does not have a lens", .x)
      )
    }
  )
})

testthat::test_that("all v_stype types have a (list-like) lens", {
  purrr::walk(
    .x = gsub("^v_", "", names(methods::getClass("v_stype")@subclasses)),
    .f = ~ {
      expect_true(
        paste0(.x, "_l") %in% stype_lenses,
        info = sprintf("%s does not have a lens", .x)
      )
    }
  )
})

testthat::test_that("all roles slots have a (list-like) lens", {
  purrr::walk(
    .x = valid_roles,
    .f = ~ {
      expect_true(
        paste0(.x, "_l") %in% stype_lenses,
        info = sprintf("%s does not have a lens", .x)
      )
    }
  )
})


testthat::test_that("(list-like) lens work", {

  # TODO: a more comprehensive set of tests would be good
  tester <- list(
    v_binary(),
    v_binary(context = context(purpose = purpose(
      study_role = "outcome",
      tags = "primary"
    ))),
    v_continuous(),
    v_count(),
    v_count(context = context(purpose = purpose(
      study_role = "outcome",
      tags = "primary"
    )))
  )

  expect_length(view(tester, binary_l), 2L)
  expect_length(view(tester, continuous_l), 1L)
  expect_length(view(tester, count_l), 2L)
  expect_length(view(tester, tag_l("primary")), 2L)
  expect_length(view(tester, count_l %.% tag_l("primary")), 1L)
  expect_length(view(tester, tag_l("secondary")), 0L)

  expect_equal(
    view(
      set(tester, tag_l("primary"), list(v_binary(), v_continuous_nonneg())),
      tag_l("primary")
    ),
    # Tags are mutable! So tagged elements can be replaced with selements
    # without the same tags.
    list()
  )
})

testthat::test_that("context lens view and set", {
  x1 <- v_binary(context = context(
    derivation = "some derivation",
    long_label = "some long label",
    short_label = "some short label",
    description = "some description",
    purpose = purpose(study_role = "outcome"),
    security_type = "some security type"
  ))

  purrr::walk2(
    .x = list(
      derivation_l,
      short_label_l,
      long_label_l,
      description_l,
      security_type_l
    ),
    .y = c(
      "some derivation",
      "some short label",
      "some long label",
      "some description",
      "some security type"
    ),
    .f = ~ {
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

  # expect_equal(view(x1, tags_l), "")
  # x2 <- set(x1, tags_l, c("primary", "something"))
  # expect_equal(view(x2, tags_l), c("primary", "something"))
  # expect_true(is_tagged(x2, "primary"))
})


test_that("data_summary lens works", {
  x <- v_binary()
  expect_is(view(x, data_summary_l), "data_summary")
  
  ds <- data_summary(list(m = 1))
  
  expect_equal(view(set(x, data_summary_l, ds), data_summary_l), ds)
})
