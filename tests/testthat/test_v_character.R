testthat::context("Testing v_character class")

stype_tester(
  v_type         = "v_character",
  canonical_type = "character",
  generator      = function() { c("This", "is", "some", "vector") },
  error_generators = list()
)

test_that("v_character class descriptions update appropriately", {
  x1 <- v_character(c("This", "is", "some", "some", "", "vector"))
  x2 <- v_character(c("This", "is", "another", "but", "EVENLONGER", "vector"))
  
  nuq  <- get_data_summary(x1, "n_unique")
  max1 <- get_data_summary(x1, "max_char")
  min1 <- get_data_summary(x1, "min_char")
  expect_equivalent(max1, 6)
  expect_equivalent(min1, 0)
  expect_equivalent(nuq, 5)

})
  