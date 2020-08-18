
stype_tester(
  v_type         = "v_continuous_nonneg",
  canonical_type = "numeric",
  generator      = function() { abs(rnorm(5)) },
  error_generators = list(function() { c("A", "B") })
)



# test_that(
#   "v_continuous_nonneg operations work",
#   {
#     # TODO: more checks
#   }
# )
