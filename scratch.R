library(dplyr)

library(DenverSugar)
x <- variable(c(TRUE, FALSE, TRUE, TRUE, NA), 
              short_label = "A test variable",
              .descriptors = list(xx = function(x, ...) length(x)))
x@description

x <- variable(c(TRUE, FALSE, TRUE, TRUE, NA), 
              short_label = "A test variable")
getDescriptors(x@.Data)
x@description

x <- variable(c(TRUE, FALSE, TRUE, TRUE, FALSE), 
              short_label = "A test variable")

describe(x, g = c("A", "A", "A", "B", "B"), w = c(2, 2, 2, 1, 1))


x2 <- variable(rnorm(100))
describe(x2)





ff <- function(x){
  purrr::map_dfr(x, ~ tibble::as_tibble(.x@description))
}
ff(z1)

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


nobs <- list(
  fun     = function(x, g, w, ...) length(x),
  label   = "Number of observations",
  printer = function(var, val) sprintf("%s has %s observations", var, val)
)

mean_sd <- list(
  fun     = function(x, g, w, ...) list(mean = mean(x), sd = sd(x)),
  label   = "Number of observations",
  printer = function(var, val) sprintf("%s (%s)", val[["mean"]], val[["sd"]])
)
