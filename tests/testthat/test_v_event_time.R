testthat::context("Testing v_event_time class")

test_that(
  "v_event_time class behaves itself",
  {
    x1 <- v_event_time(abs(rnorm(5)))
    x2 <- v_event_time(abs(rnorm(5)))
    sx1 <- x1[1:2]
    expected_attrs <- c("internal_name", "data_summary", "context")     
    
    expect_error(v_event_time(c(-1, -1.1)))
    expect_true(inherits(x1, "v_event_time"))
    expect_true(inherits(sx1, "v_event_time"))
    expect_true(all(expected_attrs%in% names(attributes(x1))))
    expect_true(all(expected_attrs%in% names(attributes(sx1))))
    expect_true(inherits(vctrs::vec_c(x1, x2), "v_event_time"))
    expect_true(inherits(c(x1, x2), "v_event_time"))
    
    expect_is(as_canonical(x1), "numeric")
  }
)

test_that(
  "v_event_time operations work",
  {
    # TODO: more checks
  }
)
