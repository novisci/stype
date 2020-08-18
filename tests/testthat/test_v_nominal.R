
stype_tester(
  v_type         = "v_nominal",
  canonical_type = "factor",
  generator      = function() { factor(c(rep(LETTERS[1:3], 3), NA)) },
  error_generators = list(function() { c(1.25, 6) })
)

test_that("v_nomimal class descriptions update appropriately", {
  x1 <- v_nominal(factor(c(rep(LETTERS[1:3], 3), NA)))
  x2 <- v_nominal(factor(c(rep(LETTERS[3:6], 3))))
  
  tab1  <- get_data_summary(x1, "table")
  ptab1 <- get_data_summary(x1, "ptable")
  expect_equivalent(as.numeric(tab1), c(3, 3, 3, 1))
  expect_equivalent(as.numeric(ptab1), c(0.3, 0.3, 0.3, 0.1))
  
  tab2  <- get_data_summary(x1[1:2], "table")
  ptab2 <- get_data_summary(x1[1:2], "ptable")
  expect_equivalent(as.numeric(tab2), c(1, 1, 0, 0))
  expect_equivalent(as.numeric(ptab2), c(0.5, 0.5, 0, 0))
  
  tab3  <- attr(c(x1, x2), "data_summary")[["table"]]
  expect_equivalent(as.numeric(tab3), c(3, 3, 6, 3, 3, 3, 1))
  
})
  