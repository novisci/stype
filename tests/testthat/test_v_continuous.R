
stype_tester(
  v_type         = "v_continuous",
  canonical_type = "numeric",
  generator      = function() { rnorm(5) },
  error_generators = list(function() { c("A", "B") })
)



test_that(
  "v_continuous operations work",
  {
    x1 <- v_continuous(rnorm(5))
    expect_s3_class(x1 + 5, "v_continuous")
    # TODO: more checks
  }
)
