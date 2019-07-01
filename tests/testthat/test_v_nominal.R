testthat::context("Testing v_nomimal class")

test_that("v_nominal class basically behaves", {
  x1 <- v_nominal(factor(c(rep(LETTERS[1:3], 3), NA)))
  x2 <- v_nominal(factor(c(rep(LETTERS[3:6], 3))))

  # expect_s3_class(x1, "v_nomimal")
  expect_true(inherits(x1, "v_nominal"))
  expect_s3_class(x1[1:2], "v_nominal")
  expect_true(inherits(x1[1:2], "v_nominal"))
  expect_s3_class(vctrs::vec_c(x1, x2), "v_nominal")
  expect_true(inherits(vctrs::vec_c(x1, x2), "v_nominal"))
})

test_that("v_nomimal class descriptions update appropriately", {
  x1 <- v_nominal(factor(c(rep(LETTERS[1:3], 3), NA)))
  x2 <- v_nominal(factor(c(rep(LETTERS[3:6], 3))))
  
  tab1  <- attr(x1, "data_summary")[["table"]]
  ptab1 <- attr(x1, "data_summary")[["ptable"]]
  expect_equivalent(as.numeric(tab1), c(3, 3, 3, 1))
  expect_equivalent(as.numeric(ptab1), c(0.3, 0.3, 0.3, 0.1))
  
  tab2  <- attr(x1[1:2], "data_summary")[["table"]]
  ptab2 <- attr(x1[1:2], "data_summary")[["ptable"]]
  expect_equivalent(as.numeric(tab2), c(1, 1, 0, 0))
  expect_equivalent(as.numeric(ptab2), c(0.5, 0.5, 0, 0))
  
  tab3  <- attr(c(x1, x2), "data_summary")[["table"]]
  expect_equivalent(as.numeric(tab3), c(3, 3, 6, 3, 3, 3, 1))
  
})
  