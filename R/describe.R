#------------------------------------------------------------------------------#
# Defines the following
#
# - useful class unions 
# - getDescriptors: a method that gets descriptors
# - .describe: the internal method for applying a descriptor
# - describe: applies a descriptor to a variable

# Classes used internally to define methods for signatures with possibly missing
# arguments.

setClassUnion("groupVar",        c("character", "factor"))
setClassUnion("maybeMissing",    c("missing", "NULL"))
setClassUnion("maybeGroup",      c("maybeMissing", "groupVar"))
setClassUnion("maybeWeight",     c("maybeMissing", "numeric"))
setClassUnion("maybeDescriptor", c("missing", "NULL", "list"))

#' Descriptor
#'
#' A function that returns a list of functions to be applied to a variable
#' 
#' @param x a \linkS4class{variable} or vector
#' @importFrom stats IQR median sd quantile
#' @export

setGeneric("getDescriptors", function(x) standardGeneric("getDescriptors"))

#' @rdname getDescriptors
#' @export

setMethod(
  f          = "getDescriptors",
  signature  = "variable",
  definition = function(x){
    append(
      list(
        # TODO insert useful summary stats that apply across all variables here
        n            = function(x, ...) length(x),
        has_missing  = function(x, ...) anyNA(x),
        n_nonmissing = function(x, ...) sum(!is.na(x)),
        n_missing    = function(x, ...) sum(is.na(x)),
        R_class      = function(x, ...) class(x@.Data),
        R_type       = function(x, ...) typeof(x@.Data)
      ),
      # TODO create methods for each data type below
      getDescriptors(x@.Data))
  }
)

#' @rdname getDescriptors
#' @export

setMethod(
  f          = "getDescriptors",
  signature  = "logical",
  definition = function(x){
    list(
      proportion = function(x, ...) mean(x, na.rm = TRUE)
    )
  }
)

# @rdname getDescriptors
# @export
# TODO: add support for factors and Dates
# setMethod(
#   f          = "getDescriptors",
#   signature  = "factor",
#   definition = function(x){
#     list(
#       table  = function(x, ...) table(x, useNA = "ifany"),
#       levels = function(x, ...) levels(x)
#     )
#   }
# )

#' @rdname getDescriptors
#' @export

setMethod(
  f          = "getDescriptors",
  signature  = "character",
  definition = function(x){
    list(
      n_unique = function(x, ...) length(unique(x)),
      max_char = function(x, ...) max(nchar(x)),
      min_char = function(x, ...) min(nchar(x))
    )
  }
)

#' @rdname getDescriptors
#' @export

setMethod(
  f          = "getDescriptors",
  signature  = "numeric",
  definition = function(x){
    list(
      mean   = function(x, ...) mean(x, na.rm = TRUE),
      sd     = function(x, ...) sd(x, na.rm = TRUE),
      median = function(x, ...) median(x, na.rm = TRUE),
      iqr    = function(x, ...) IQR(x, na.rm = TRUE)
    )
  }
)

# @rdname getDescriptors
# @export
# setMethod(
#   f          = "getDescriptors",
#   signature  = "groupVar",
#   definition = function(x){
#     list(
#       smd = function(x, g, w = NULL) SugarMaryDenver::smd(x, g, w, na.rm = TRUE)
#     )
#   }
# )



#' @rdname getDescriptors
#' @export

setMethod(
  f          = "getDescriptors",
  signature  = "NULL",
  definition = function(x){ NULL }
)

# Describe a variable (internal method)
# 
# The internal method for applying a descriptive function on x and, optionally, 
# g and/or w. The methods define the signature patterns for applying the 
# functions.

setGeneric(".describe", function(f, x, g, w, ...) standardGeneric(".describe"))

.describeMethods <- list(
  list(
    sig = c("function", "variable", "maybeMissing", "maybeMissing"),
    bod = quote(f(x, ...))
  ),
  list(
    sig = c("function", "variable", "groupVar", "maybeMissing"),
    bod = quote(f(x = x, g = g, ...))
  ),
  list(
    sig = c("function", "variable", "maybeMissing", "numeric"),
    bod = quote(f(x = x, w = w, ...))
  ),
  list(
    sig = c("function", "variable", "groupVar", "numeric"),
    bod = quote(f(x = x, g = g, w = w, ...) )
  )
)

purrr::walk(
  .x = .describeMethods,
  .f = function(l){
    
    def <- getGeneric(".describe")
    body(def) <- l$bod
    
    setMethod(f = ".describe", signature  = l$sig, definition = def)
  }
)

#' Describe a variable
#' 
#' @param x a vector a values
#' @param g a vector a groupings (optional)
#' @param w a vector of weights (optional)
#' @param .descriptors an (optional) list of lambda functions
#' @param ... additional arguments
#' @importFrom purrr reduce map
#' @importFrom stats setNames
#' @importFrom methods is
#' @export

setGeneric(
  name = "describe", 
  def  = function(x, g = NULL, w = NULL, .descriptors, ...) standardGeneric("describe")
)

#' @rdname describe
#' @export

setMethod(
  f          = "describe",
  signature  = c("variable", "maybeGroup", "maybeWeight", "maybeDescriptor"),
  definition = function(x, g, w, .descriptors, ...){
    
    # TODO: add described() method which detects whether a variable has been 
    # previously describe()d using the same arguments. If it has, then simply
    # return the description slot rather than carrying out computations.
    
    desc <- if (missing(.descriptors) || methods::is(.descriptors, "maybeMissing")){
      purrr::reduce(
        .x = list(x, g, w),
        .f = ~ append(.x, getDescriptors(.y)),
        .init = NULL) 
    } else {
      .descriptors
    }
    
    description(
      purrr::map(
        .x = desc,
        .f = function(f) .describe(f, x = x, g = g, w = w, ...))
    )
  }
)

