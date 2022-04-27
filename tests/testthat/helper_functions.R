
# Get the available methods defined for a stype class for a given Group.
get_methods_for_group <- function(grp) {
  function(v_type) {
    patt <- sprintf("\\.%s", v_type)
    x <- gsub(patt, "", as.character(methods(class = v_type)))
    intersect(getGroupMembers(grp), x)
  }
}

get_arith_methods <- get_methods_for_group(Arith)
get_math_methods <- get_methods_for_group(Math)
get_summary_methods <- get_methods_for_group(Summary)

# Compare a summary method for a stype class
check_summary_method <- function(method, x, y, v_type, ...) {
  dots <- list(...)
  t1 <- do.call(method, c(list(x), dots))
  t2 <- do.call(method, c(list(y), dots))
  msg <- sprintf(
    "%s(x) and %s(y) are not equal for %s",
    method, method, v_type
  )
  expect_equal(t1, t2, label = msg)
}

# Check valid arithmetic operations
check_arith_op_valid <- function(op, x, y, e) {
  # browser()
  expect_identical(op(x, y), e)
}

# Check invalid arithmetic operations
check_arith_op_invalid <- function(op, x, y) {
  expect_error(op(x, y))
}

# Check arith
check_arith <- function(xs, ys, valid = list(), invalid = list()) {
  inputs <- purrr::cross2(xs, ys)

  purrr::walk(
    .x = inputs,
    .f = ~ {
      x <- .x[[1]]
      y <- .x[[2]]
      purrr::walk(
        .x = valid,
        .f = ~ check_arith_op_valid(op = .x$op, x, y, e = .x$e)
      )

      purrr::walk(
        .x = invalid,
        .f = ~ check_arith_op_invalid(op = .x$op, x, y)
      )
    }
  )
}

# check that sum(x, y), mean(x, y) fails
# here x, y are of the same stype (x == y), but it should also
# fail for all other stypes y
check_summary_err <- function(xs) {
  expect_error(sum(xs, xs))
  # expect_error(mean(xs, xs))
}

# A function which generates test for a stype class
stype_tester <- function(v_type, canonical_type, generator, error_generators) {

  # Generate two vectors
  # TODO: I'm not in love with this approach of passing a list (for types with
  # mulitple inputs or a function
  if (is.list(generator)) {
    g1 <- g2 <- generator
  } else {
    g1 <- list(generator())
    g2 <- list(generator())
  }

  x1 <- do.call(v_type, args = g1)
  # compute x1 where the auto compute summary is the opposite of the default
  x1_autoopp <- do.call(
    v_type,
    args = c(g1, list(auto_compute_summary = !auto_compute_default))
  )
  x2 <- do.call(v_type, args = g2)

  empty <- do.call(v_type, args = list())
  empty_autoopp <-
    do.call(v_type, args = list(auto_compute_summary = !auto_compute_default))

  ctxt1 <- context(short_label = "test label", long_label = "test label")
  ctxt2 <- context(short_label = "diff label", long_label = "diff label")
  x1c <- do.call(v_type, args = append(g1, list(
    internal_name = "test",
    context = ctxt1
  )))
  x2c <- do.call(v_type, args = append(g2, list(
    internal_name = "test",
    context = ctxt2
  )))

  test_that(
    sprintf("%s inheritance is correct", v_type),
    {
      ### Check inheritance of v_type
      expect_true(inherits(x1, v_type))
      # after subsetting
      expect_true(inherits(x1[1:2], v_type))
      # after vec_c
      expect_true(inherits(vctrs::vec_c(x1, x2), v_type))
      # after c
      expect_true(inherits(c(x1, x2), v_type))
    }
  )

  if (length(error_generators) > 0) {
    test_that(
      sprintf("%s should give error on bad inputs", v_type),
      {
        ### Check that errors occur when given bad inputs
        purrr::walk(
          .x = error_generators,
          .f = function(f) {
            dat <- f()
            expect_error(
              do.call(v_type, args = list(dat)),
              info = sprintf(
                "%s did not give error with inputs: %s",
                v_type,
                paste(dat, collapse = ", ")
              )
            )
          }
        )
      }
    )
  }

  test_that(
    sprintf("%s should retain attributes", v_type),
    {
      ### Check that all expected attributes are retained
      expected_attrs <- c("internal_name", "data_summary", "context")
      expect_true(all(expected_attrs %in% names(attributes(x1))))
      expect_true(all(expected_attrs %in% names(attributes(x1[1:2]))))
      expect_true(all(expected_attrs %in% names(attributes(c(x1, x2)))))
    }
  )

  test_that(
    sprintf("as_canonical(%s) should return %s", v_type, canonical_type),
    {
      ### Check as_canonical returns the right type
      expect_is(as_canonical(x1), canonical_type)
    }
  )

  test_that(
    sprintf("internal_names are retained in %s", v_type),
    {
      expect_equal(get_internal_name(x1c[1:2]), "test")
      expect_equal(get_internal_name(vctrs::vec_c(x1c, x1c)), "test")
      expect_equal(get_internal_name(c(x1c, x1c)), "test")
    }
  )

  test_that(
    sprintf("contexts are retained in %s", v_type),
    {
      # Check context is there
      ctxt <- get_context(x1c)
      expect_is(ctxt, "context")

      # Check context elements are as expected
      expect_equal(get_short_label(x1c), "test label")
      expect_equal(get_short_label(x1c[1:2]), "test label")

      # Check contexts are handled correctly when combining vectors
      expect_equal(get_short_label(c(x1c, x1c)), "test label")

    }
  )

  test_that(
    sprintf("concatenating works appropriately in %s", v_type),
    {
      # Concatenating fails with different contexts
      expect_error(c(x1c, x2c))

      # Concatenating succeeded with same context; different data
      expect_is(c(x1c, x1c[1]), v_type)

    }
  )

  test_that(
    sprintf("purposes can be modified for %s", v_type),
    {
      expect_equal(
        view(add_study_role(x1, "outcome"), study_role_l),
        "outcome"
      )

      expect_equal(
        view(add_study_role(x1, c("outcome", "covariate")), study_role_l),
        c("outcome", "covariate")
      )

      expect_error(add_study_role(x1, "blah"))

      expect_equal(
        view(add_tags(x1, LETTERS[1:5]), tags_l),
        LETTERS[1:5]
      )

      x2 <- add_tags(add_study_role(x1, "outcome"), LETTERS[1:5])

      expect_equal(
        view(remove_study_role(x2, "outcome"), study_role_l),
        character()
      )

      expect_equal(
        view(remove_tags(x2, LETTERS[2:5]), tags_l),
        LETTERS[1]
      )
    }
  )

  test_that(
    sprintf("%s can be cast to a character", v_type),
    {
      expect_is(as.character(x1), "character")
    }
  )

  test_that("get_internal_name should work", {
    expect_equal(get_internal_name(x1c), "test")
  })


  test_that("auto_compute setting is not changed on subset", {
    expect_equal(
      view(x1, auto_compute_summary_l),
      view(x1[1], auto_compute_summary_l)
    )

    expect_equal(
      view(x1_autoopp, auto_compute_summary_l),
      view(x1_autoopp[1], auto_compute_summary_l)
    )
  })

  test_that("auto_compute favors the default on c()", {
    # TODO: using R 3.6.3 on CI getting this error
    #   Internal error in `vec_proxy_assign_opts()`: `proxy` of type `integer`
    #     incompatible with `value` proxy of type `character`.
    # Don't get this error locally with R 4.1+ and vctrs 0.3.8, so skipping
    # this test on CI for now
    skip_on_ci()
    expect_equal(
      view(c(empty, empty), auto_compute_summary_l),
      auto_compute_default
    )

    skip_on_ci()
    expect_equal(
      view(c(empty, empty_autoopp), auto_compute_summary_l),
      auto_compute_default
    )

    skip_on_ci()
    expect_equal(
      view(c(empty_autoopp, empty), auto_compute_summary_l),
      auto_compute_default
    )

    skip_on_ci()
    expect_equal(
      view(c(empty_autoopp, empty_autoopp), auto_compute_summary_l),
      !auto_compute_default
    )
  })

  # If the class has a vec_math method OR has any summary methods defined
  # separately from vec_math, then test that the methods defined separately
  # and a default set of methods return the same summary values with vectors
  # with auto compute on vs off.

  summary_methods_to_check <- c("sum", "mean")
  if (any(grepl(v_type, as.character(methods(vctrs::vec_math, class = v_type)))) ||
    length(get_summary_methods(v_type)) > 0) {
    test_that("summary methods return same values for auto_compute on/off by default", {
      purrr::walk(
        .x = union(get_summary_methods(v_type), c("sum", "mean")),
        .f = ~ check_summary_method(.x, x1, x1_autoopp, v_type)
      )
    })

    test_that("summary methods return same values for auto_compute on/off with na.rm = FALSE", {
      purrr::walk(
        .x = union(get_summary_methods(v_type), c("sum", "mean")),
        .f = ~ check_summary_method(.x, c(x1, NA), c(x1_autoopp, NA), v_type, na.rm = FALSE)
      )
    })

    test_that("summary methods return same values for auto_compute on/off with na.rm = TRUE", {
      purrr::walk(
        .x = union(get_summary_methods(v_type), c("sum", "mean")),
        .f = ~ check_summary_method(.x, c(x1, NA), c(x1_autoopp, NA), v_type, na.rm = TRUE)
      )
    })
  }
}

check_footer <- function(x, v_type, summaries) {
  reg_sums <- `if`(
    !is.null(summaries),
    paste0(paste0(summaries, " = \\d*.*"), collapse = ""),
    NULL
  )

  v1 <- v_type(x)
  out_reg <- sprintf("<%s\\[%s\\]>\\n.*\\n%s$", vec_ptype_full(v1), length(v1), reg_sums)
  expect_output(print(v1), regexp = out_reg)

  v2 <- v_type(x, auto_compute_summary = F)
  out_reg <- sprintf("<%s\\[%s\\]>\\n ?\\[1\\](\\w|\\d|\\.| )*$", vec_ptype_full(v2), length(v2))
  expect_output(print(v2), regexp = out_reg)
}
