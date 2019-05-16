
setGeneric("describe", function(x) standardGeneric("describe"))
setGeneric(".describe", function(x) standardGeneric(".describe"))
setGeneric(".summarise", function(x, ...) standardGeneric(".summarise"))

setMethod(
  f = "describe",
  signature  = "variableUnion",
  definition = function(x){
    cat(
      sprintf("This variable is an %s with %s observations.\n", class(x), length(x)),
      sprintf("Summary value: %s", .summarise(x))
    )
    
  }
)

setMethod(
  f = ".describe",
  signature = "outcome",
  definition = function(x){
    any(x)
  }
)

setMethod(
  f = ".describe",
  signature = "covariate",
  definition = function(x){
    all(x)
  }
)

setMethod(
  f          = ".summarise",
  signature  = "numeric",
  definition = function(x, funs = list(mean = mean, sd = sd)) {
    purrr::map(.x = funs, ~ .x(x))
  }
)

setMethod(
  f          = ".summarise",
  signature  = "logical",
  definition = function(x, funs = list(mean = mean, sd = sd)) {
    purrr::map(.x = funs, ~ .x(x))
  }
)




.summarise(rnorm(100))
.summarise(rnorm(100), list(median = median, iqr = IQR))
.summarise(c(TRUE, FALSE, TRUE, FALSE))



test <- tibble(
  x = outcome(c(TRUE, FALSE),   label = "myOutcome is great"),
  y = covariate(c(TRUE, FALSE), label = "myCovariate is great")
) 


describe(test$x)
describe(test$y)
