library(dplyr)


x <- variable(c(TRUE, FALSE, TRUE, TRUE, NA), short_label = "A test variable")
descriptor(x)
descriptor(x) %>% names()

describe(x)

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

z1 %>%
  {
    df <- .
    . %>% mutate(x = 1) -> out
    metadata(out) <- metadata(df)
    out
  }

  mutate(
    # x    = cumsum(x),
    test = cumsum(x)
  ) -> hold

hold$x
hold$test
z2 <- tibble::tibble(
  x = variable(rnorm(5), short_label = "A test variable")
)
z2



Hmisc::label(x@.Data) <- "test"
, "label") <- test
tibble::tibble(x) %>% glimpse_labels()
z2
z2[[1]]

make_cohort_tibble <- function(l){
  tibble::new_tibble(l, nrow = length(l[[1]]))
}


#' #'
#' #'
#' 
#' outcome <- setClass(
#'   "outcome",
#'   slots   =  c("type" = "character"),
#'   contains = "variable"
#' )
#' 
#' #'
#' #'
#' 
#' covariate <- setClass(
#'   "covariate",
#'   contains = "variable"    
#' )
#' 
#' 
#' setClassUnion("variableUnion", members = c("outcome", "covariate"))


.summarise(rnorm(100))
.summarise(rnorm(100), list(median = median, iqr = IQR))
.summarise(c(TRUE, FALSE, TRUE, FALSE))



test <- tibble(
  x = outcome(c(TRUE, FALSE),   label = "myOutcome is great"),
  y = covariate(c(TRUE, FALSE), label = "myCovariate is great")
) 


describe(test$x)
describe(test$y)

