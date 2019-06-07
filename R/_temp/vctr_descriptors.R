
apply_descriptors <- function(x, descriptors = c(), ...){
  
  x <- vctrs::vec_data(x)
  if(is.null(descriptors)){
    # Return empty description
    return(description())
  }
  
  browser()
  
  purrr::reduce(
    .x = descriptors,
    .f = function(d, f){
      f <- match.fun(f)
      append(d, list(f(x, ...)))
    },
    .init = list()
  )
}


describe_mean_var <- function(x){
  list(
    mean    = mean(x), 
    sd      = sd(x)
  ) -> out
  
  class(out) <- "test"
  out
}

print.test <- function(x, ...){
  sprintf("Mean (SD): %.3f (%.4f)", x[["mean"]], x[["sd"]])
}

describe_quantiles <- function(x){
  qs <- c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99)
  out <-purrr::map_dbl(
    .x = qs,
    .f = ~ quantile(x, .x, na.rm = TRUE)
  ) 
  
  setNames(out, nm = paste0("q", qs))
}

description <- list