
stype_tester(
  v_type         = "v_ordered",
  canonical_type = "ordered",
  generator      = function() { factor(c(rep(LETTERS[1:3], 3), NA), ordered = TRUE) },
  error_generators = list(function() { c(1.25, 6) })
)

test_that("v_ordered class descriptions update appropriately", {
  x1 <- v_ordered(factor(c(rep(LETTERS[1:3], 3), NA), ordered = TRUE))
  x2 <- v_ordered(factor(c(rep(LETTERS[3:6], 3)),  ordered = TRUE))
  
  tab1  <- get_data_summary(x1, "table")
  ptab1 <- get_data_summary(x1, "ptable")
  expect_equivalent(as.numeric(tab1), c(3, 3, 3, 1))
  expect_equivalent(as.numeric(ptab1), c(0.3, 0.3, 0.3, 0.1))
  
  tab2  <- get_data_summary(x1[1:2], "table")
  ptab2 <- get_data_summary(x1[1:2], "ptable")
  expect_equivalent(as.numeric(tab2), c(1, 1, 0, 0))
  expect_equivalent(as.numeric(ptab2), c(0.5, 0.5, 0, 0))
  
  tab3  <- get_data_summary(c(x1, x2), "table")
  expect_equivalent(as.numeric(tab3), c(3, 3, 6, 3, 3, 3, 1))
  
})
  
test_that(
  "misc tests for v_ordered", {
    
    char_o <- factor(LETTERS[1:3], ordered = T)
    ord1 <- v_ordered(char_o)
    ord2 <- v_ordered(factor(LETTERS[4:6], ordered = T))
    ord0 <- v_ordered()
    ## test footer
    expect_output(print(ord1))
    
    ## test type sum
    expect_equal(type_sum(ord1), "ord")
    
    ## Casting
    expect_error(as_ordered(factor(LETTERS[1:3])))
    expect_equal(as_ordered(char_o), ord1)
    
    ## Coercion
    expect_equal(c(ord1, "B"), c(levels(ord1), "B"))
    expect_type(c("B", ord1), "character")
    expect_s3_class(c(ord1, ord2), "v_ordered")
    expect_equal(levels(c(ord1, ord2)), LETTERS[1:6])
    expect_s3_class(c(ord1, ord0), "v_ordered")
  }
)
    