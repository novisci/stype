## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(stype)

x <- v_binary(c(TRUE, TRUE, TRUE, FALSE))

str(x)

## -----------------------------------------------------------------------------
x
vctrs::vec_data(x)

## -----------------------------------------------------------------------------
is_binary(x)

## -----------------------------------------------------------------------------
mean(x)
sum(x)

# sum(x, x) # See? very experimental


## ---- error=TRUE--------------------------------------------------------------
# What do you mean you want to add binary and integer?
x + 2L 

# R's base types are not so safe
vctrs::vec_data(x) + 2L

## -----------------------------------------------------------------------------
!x
all(x)
any(x)

## ---- error=TRUE--------------------------------------------------------------
# vectors can be combined and ...
# subsetting maintains and updates attributes
c(x, !x[1:3])

# But ...
c(x, v_binary(context = context(purpose = purpose(study_role = "other"))))


## -----------------------------------------------------------------------------
library(dplyr)
library(tibble)
n <- 100 

make_context <- function(role){
  context(purpose = purpose(study_role = role))
}

covariates <-
purrr::map(
    .x  = purrr::set_names(1:10, paste0("x", 1:10)), 
    .f = ~ v_binary(as.logical(rbinom(n, 1, 0.25)), 
                    context = make_context("covariate"))
)

dt <- tibble(
  y1 = v_binary(as.logical(rbinom(n, 1, 0.25)), context = make_context("outcome")),
  y2 = v_event_time(runif(n, 1, 100), context = make_context("outcome")),
  y3 = v_continuous(rnorm(n), context = make_context("outcome")),
  !!! covariates
)

dt

## -----------------------------------------------------------------------------
dt %>% select_if(is_binary)

## -----------------------------------------------------------------------------
dt %>% select_if(is_outcome)

