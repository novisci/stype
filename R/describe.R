#------------------------------------------------------------------------------#
# Defines the following
# - descriptor: a method that returns an unevaluated description for a variable
# - describe: applies a descriptor to a variable

# Classes used internally to define methods for signatures with possibly missing
# arguments.

setClassUnion("groupVar",     c("character", "factor"))
setClassUnion("maybeMissing", c("missing", "NULL"))
setClassUnion("maybeGroup",   c("maybeMissing", "groupVar"))
setClassUnion("maybeWeight",  c("maybeMissing", "numeric"))

#' Descriptor
#'
#' A function that returns a list of functions to be applied to a variable
#' 
#' @param x a \linkS4class{variable} or vector
#' @export

setGeneric("descriptor", function(x) standardGeneric("descriptor"))

#' @rdname descriptor
#' @export

setMethod(
  f          = "descriptor",
  signature  = "variable",
  definition = function(x){
    append(
      list(
        # TODO insert useful summary stats that apply across all variables here
        n            = function(x, ...) length(x),
        has_missing  = function(x, ...) anyNA(x),
        n_nonmissing = function(x, ...) sum(!is.na(x)),
        n_missing    = function(x, ...) sum(is.na(x))
      ),
      # TODO create methods for each data type below
      descriptor(x@.Data))
  }
)

#' @rdname descriptor
#' @export

setMethod(
  f          = "descriptor",
  signature  = "logical",
  definition = function(x){
    list(
      proportion = function(x, ...) mean(x, na.rm = TRUE)
    )
  }
)

#' @rdname descriptor
#' @export

setMethod(
  f          = "descriptor",
  signature  = "groupVar",
  definition = function(x){
    list(
      smd = function(x, g, w = NULL) SugarMaryDenver::smd(x, g, w, na.rm = TRUE)
    )
  }
)

#' @rdname descriptor
#' @export

setMethod(
  f          = "descriptor",
  signature  = "maybeWeight",
  definition = function(x){ NULL }
)

#' @rdname descriptor
#' @export

setMethod(
  f          = "descriptor",
  signature  = "NULL",
  definition = function(x){ NULL }
)

# Describe a variable (internal method)
# 
# The internal method for applying a descriptive function on x and, optionally, 
# g and/or w. The methods define the signature patterns for applying the 
# functions.

setGeneric(".describe", function(f, x, g, w, ...) standardGeneric(".describe"))

setMethod(
  f          = ".describe",
  signature  = c("function", "variable", "NULL", "NULL"),
  definition = function(f, x, g, w, ...){ f(x, ...) } )

setMethod(
  f          = ".describe",
  signature  = c("function", "variable", "groupVar", "NULL"),
  definition = function(f, x, g, w, ...){ f(x = x, g = g, ...) } )

setMethod(
  f          = ".describe",
  signature  = c("function", "variable", "NULL", "numeric"),
  definition = function(f, x, g, w, ...){ f(x = x, w = w, ...) } )

setMethod(
  f          = ".describe",
  signature  = c("function", "variable", "groupVar", "numeric"),
  definition = function(f, x, g, w, ...){ f(x = x, g = g, w = w, ...) })

#' Describe a variable
#' 
#' @param x a vector a values
#' @param g a vector a groupings (optional)
#' @param w a vector of weights (optional)
#' @param ... additional arguments
#' @importFrom purrr reduce map
#' @importFrom stats setNames
#' @export

setGeneric(
  name = "describe", 
  def  = function(x, g = NULL, w = NULL, ...) standardGeneric("describe")
)

#' @rdname describe
#' @export

setMethod(
  f          = "describe",
  signature  = c("variable", "maybeGroup", "maybeWeight"),
  definition = function(x, g, w, ...){
    
    purrr::reduce(
      .x = list(x, g, w),
      .f = ~ append(.x, descriptor(.y)),
      .init = NULL
    ) -> desc
    
    description(
      stats::setNames(
        purrr::map(
          .x = desc,
          .f = function(f) .describe(f, x = x, g = g, w = w, ...)),
        nm = names(desc)
      )
    )
  }
)

