library(DenverSugar)
xx <- count(1L:10000L)
xx
attr(xx, "desc")
describe(xx)

describe(xx[1:100])

zz <- tibble::tibble(x = xx)
attr(zz$x, "desc")
library(magrittr)
zz %>% 
  dplyr::mutate(
    g = rep(c(TRUE, FALSE), each = 5000)
  ) %>%
  dplyr::group_split(g) %>%
  purrr::map_dfr(~ describe(.x$x))

summarise(
  mean = mean(x)
)

attr(zzz$x, "desc")
zz <- atibble(
  name = "A test analytic file",,
  label = "",
  x = xx
)

describe(zz[1:1100, ])
library(DenverSugar)
x <- variable(c(TRUE, FALSE, TRUE, TRUE, NA), 
              short_label = "A test variable",
              .descriptors = list(xx = function(x, ...) length(x)))
x@description

x <- variable(c(TRUE, FALSE, TRUE, TRUE, NA), 
              short_label = "A test variable")

x <- variable(LETTERS[1:2], 
              short_label = "A test variable")
x@description
getDescriptors(x@.Data)
x@description

x <- variable(c(TRUE, FALSE, TRUE, TRUE, FALSE), 
              short_label = "A test variable")

describe(x, g = c("A", "A", "A", "B", "B"), w = c(2, 2, 2, 1, 1))


x2 <- variable(rnorm(100))
describe(x2)







z1$z@description
broom::glance(z1)
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

