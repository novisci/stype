
apply_formatters <- function(x, formatters = c(), ...){
  x <- vctrs::vec_data(x)
  if(is.null(formatters)){
    # Return unformatted x
    return(x)
  }
  
  purrr::reduce(
    .x = formatters,
    .f = function(x, f){
      f <- match.fun(f)
      f(x, ...)
    },
    .init = x
  )
}
  