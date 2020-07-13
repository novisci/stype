## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(stype)
library(lenses)

## -----------------------------------------------------------------------------
x <- v_binary(c(TRUE, FALSE, TRUE, FALSE))
w <- c(10, 10, 1, 1)
describe(vctrs::vec_data(x), w = w)

