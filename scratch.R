library(DenverSugar)
library(dplyr)


example <- tibble::tibble(
  x1 = v_count(10L:1L),
  x2 = v_binary(c(NA, rep(c(TRUE, FALSE), 4), NA)),
  x3 = v_continuous(rnorm(10)),
  x4 = v_continuous_nonneg(rlnorm(10)),
  x5 = v_event_time(runif(10, min = 0, max = 10)),
  g  = rep(LETTERS[1:2], each = 5)
)

variable( v_count(10L:1L), name = "xx", short_label = "xx", long_label = "xxx")

example
glance(example)
example %>%
  select_if(is_continuous)

example %>%
  select_if(is_binary)

example %>%
  select_if(is_count)

ex_sub <- example %>%
  filter(rep(c(TRUE, FALSE), 5))

attr(ex_sub@data$x3, "desc")

example %>%
  DenverSugar::group_split(g) %>%
  purrr::map(
    .x = .,
    .f = ~ glance(.x)
  )


%>%
  .@data %>%
  purrr::map(
    .x = .$data,
    .f = ~ glance(.x)
  )


ex_sub %>% glance()
