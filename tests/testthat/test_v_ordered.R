testthat::context("Testing v_ordered class")

test_that("v_ordered class basically behaves", {
  x1 <- v_ordered(factor(c(rep(LETTERS[1:3], 3), NA), ordered = TRUE))
  x2 <- v_ordered(factor(c(rep(LETTERS[3:6], 3)), ordered = TRUE))
  sx1 <- x1[1:2]
  expected_attrs <- c("internal_name", "data_summary", "context")     
  
  # expect_s3_class(x1, "v_nomimal")
  expect_true(inherits(x1, "v_ordered"))
  expect_s3_class(sx1, "v_ordered")
  expect_true(inherits(sx1, "v_ordered"))
  expect_true(all(expected_attrs%in% names(attributes(x1))))
  expect_true(all(expected_attrs%in% names(attributes(sx1))))
  expect_s3_class(vctrs::vec_c(x1, x2), "v_ordered")
  expect_true(inherits(vctrs::vec_c(x1, x2), "v_ordered"))
  
  expect_is(as_canonical(x1), "ordered")
})

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
  