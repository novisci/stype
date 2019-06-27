testthat::context("Testing v_character class")

test_that("v_character class basically behaves", {
  x1 <- v_character(c("This", "is", "some", "vector"))
  x2 <- v_character(c("This", "is", "another", "but", "EVENLONGER", "vector"))

  # expect_s3_class(x1, "v_nomimal")
  expect_true(inherits(x1, "v_character"))
  expect_s3_class(x1[1:2], "v_character")
  expect_true(inherits(x1[1:2], "v_character"))
  expect_s3_class(vctrs::vec_c(x1, x2), "v_character")
  expect_true(inherits(vctrs::vec_c(x1, x2), "v_character"))
})

test_that("v_character class descriptions update appropriately", {
  x1 <- v_character(c("This", "is", "some", "some", "", "vector"))
  x2 <- v_character(c("This", "is", "another", "but", "EVENLONGER", "vector"))
  
  nuq  <- attr(x1, "desc")[["n_unique"]]
  max1 <- attr(x1, "desc")[["max_char"]]
  min1 <- attr(x1, "desc")[["min_char"]]
  expect_equivalent(max1, 6)
  expect_equivalent(min1, 0)
  expect_equivalent(nuq, 5)

})
  