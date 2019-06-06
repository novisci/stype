
apply_formatters <- function(x, formatters = c(), ...){
  purrr::reduce(
    .x = formatters,
    .f = function(x, f){
      f <- match.fun(f)
      f(x, ...)
    },
    .init = x
  )
}
