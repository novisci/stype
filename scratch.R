library(dplyr)

library(DenverSugar)
x <- variable(c(TRUE, FALSE, TRUE, TRUE, NA), short_label = "A test variable")
descriptor(x)
describe(x)

x <- variable(c(TRUE, FALSE, TRUE, TRUE, FALSE), short_label = "A test variable")
describe(x, g = c("A", "A", "A", "B", "B"), w = c(2, 2, 2, 1, 1))




x
z1 <- data.frame(
  x = variable(rnorm(10), short_label = "A test variable")
)
z1$x

data.frame2 <- setClass(
  "data.frame2",
  contains = "data.frame"
)



setMethod(
  f = "filter",
  signature = "data.frame2",
  function(.data, ..., .preserve = FALSE){
    .data %>%
      {
        df <- .
        . %>% dplyr::filter(..., .preserve) -> out
        out
      }
  }
)
