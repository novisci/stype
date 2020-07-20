testthat::context("Testing v_event_time class")

stype_tester(
  v_type         = "v_event_time",
  canonical_type = "numeric",
  generator      = function() { abs(rnorm(5)) },
  error_generators = list(function() { c(-1, -1.1) })
)

# test_that(
#   "v_event_time operations work",
#   {
#     # TODO: more checks
#   }
# )
