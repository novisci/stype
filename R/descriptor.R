#------------------------------------------------------------------------------#
# Defines the following
# - descriptors: a class that defines how to describe an object

# TODO: this is essentially a stub for the descriptor class. More thought needs
#       to go into the design of how descriptors work and where they are stored,
#       how they handled grouped and/or weighted data
# TODO: To implement descriptors this will require changing the internals of the
#       getDescriptors and .describe methods


#' descriptor
#'
#' Extends R's \code{\link{vector-class}} to provide objects with additional information. 
#' The vector object must be one of the following basic classes: \code{"numeric", "double", 
#' "integer", "logical", "character",}, or \code{\link{factor}}, or \code{\link{Date}}.
#'  A variable object can used as if it was a vector.
#' 
#' @slot f an anyomymous with a signature of 
#' @slot label a character description of the descriptor
#' @slot display a function for displaying the descriptor
#' @export descriptor

descriptor <- setClass(
  "descriptor",
  slots = c(
    "f"       = "function",
    "label"   = "character", 
    "display" = "function"
  )
)

# TODO : setValidity of descriptor

#------------------------------------------------------------------------------#
# Available descriptors

desc <- list(
  nobs    = list(
    f       = function(x, g, w, ...) length(x),
    label   = "Number of observations",
    display = function(val) sprintf("%s", val)
  ),
  mean_sd = list(
    f       = function(x, g, w, ...) list(mean = mean(x), sd = sd(x)),
    label   = "Mean (SD)",
    # TODO: be able to manipulate decimal precision
    display = function(val) sprintf("%.3f (%.4f)", val[["mean"]], val[["sd"]])
  )
)

# purrr::map(desc, ~ do.call("descriptor", .x))
