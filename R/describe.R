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
setClassUnion("categorical",     c("factor", "ordered"))
setClassUnion("maybeMissing",    c("missing", "NULL"))
setClassUnion("maybeGroup",      c("maybeMissing", "groupVar"))
setClassUnion("maybeWeight",     c("maybeMissing", "numeric"))
setClassUnion("maybeDescriptor", c("missing", "NULL", "list"))
setClassUnion("describable",     c("integer", "logical", "numeric", "factor",
                                   "ordered", "character", "v_rcensored"))
setClassUnion("stype",           c("v_count", "v_binary", "v_continuous", 
                                   "v_continuous_nonneg", "v_event_time",
                                   "v_nominal", "v_ordered", "v_character",
                                   "v_rcensored"))

#' Descriptor
#'
#' A function that returns a list of functions to be applied to a variable
#' 
#' @param x a vector
#' @importFrom stats IQR median sd quantile
#' @export

setGeneric("getDescriptors", function(x) standardGeneric("getDescriptors"))

standardDescriptors <-  list(
  # TODO insert useful summary stats that apply across all variables here
  n            = function(x, ...) length(x),
  has_missing  = function(x, ...) anyNA(x),
  n_nonmissing = function(x, ...) sum(!is.na(x)),
  n_missing    = function(x, ...) sum(is.na(x)),
  proportion_missing = function(x, ...) mean(is.na(x)),
  is_constant  = function(x, ...) all(x[1] == x)
)

#' @rdname getDescriptors
#' @export

setMethod(
  f          = "getDescriptors",
  signature  = "logical",
  definition = function(x){
    append(
      standardDescriptors,
      list(
        num_0      = function(x, ...) sum(!x, na.rm = TRUE),
        num_1      = function(x, ...) sum(x, na.rm = TRUE),
        proportion = function(x, ...) mean(x, na.rm = TRUE)
      ))
  }
)

#' @rdname getDescriptors
#' @export

setMethod(
  f          = "getDescriptors",
  signature  = "categorical",
  definition = function(x){
    append(
      standardDescriptors,
      list(
        table  = function(x, ...) table(x, useNA = "always"),
        ptable = function(x, ...) prop.table(table(x, useNA = "always")),
        levels = function(x, ...) levels(x)
      )
    )
  }
)

#' @rdname getDescriptors
#' @export

setMethod(
  f          = "getDescriptors",
  signature  = "character",
  definition = function(x){
    append(
      standardDescriptors,
      list(
        n_unique = function(x, ...) length(unique(x)),
        max_char = function(x, ...) if(length(x) == 0) 0 else max(nchar(x)),
        min_char = function(x, ...) if(length(x) == 0) 0 else min(nchar(x))
      )
    )
  }
)

#' @rdname getDescriptors
#' @export

setMethod(
  f          = "getDescriptors",
  signature  = "numeric",
  definition = function(x){
    
    qprobs = c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99)
    append(
      standardDescriptors,
      list(
        sum    = function(x, ...) sum(x, na.rm = TRUE),
        mean   = function(x, ...) mean(x, na.rm = TRUE),
        sd     = function(x, ...) sd(x, na.rm = TRUE),
        median = function(x, ...) median(x, na.rm = TRUE),
        iqr    = function(x, ...) IQR(x, na.rm = TRUE),
        min    = function(x, ...) { if (length(x) == 0 || all(is.na(x))) NA_real_ else min(x, na.rm = TRUE) },
        max    = function(x, ...) { if (length(x) == 0 || all(is.na(x))) NA_real_ else max(x, na.rm = TRUE) },
        qtiles = function(x, ...) quantile(x, probs = qprobs, na.rm = TRUE)
        # hist   = function(x, ...) list(ggplot2::qplot(x, geom = "histogram", ...))
      )
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
    sig = c("function", "describable", "maybeMissing", "maybeMissing"),
    bod = quote(f(x, ...))
  ),
  list(
    sig = c("function", "describable", "groupVar", "maybeMissing"),
    bod = quote(f(x = x, g = g, ...))
  ),
  list(
    sig = c("function", "describable", "maybeMissing", "numeric"),
    bod = quote(f(x = x, w = w, ...))
  ),
  list(
    sig = c("function", "describable", "groupVar", "numeric"),
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
  signature  = c("describable", "maybeGroup", "maybeWeight", "maybeDescriptor"),
  definition = function(x, g, w, .descriptors, ...){
    
    # TODO: add styped() method which detects whether a variable has been 
    # previously styped()d using the same arguments. If it has, then simply
    # return the description slot rather than carrying out computations.
    
    desc <- if (missing(.descriptors) || methods::is(.descriptors, "maybeMissing")){
      purrr::reduce(
        .x = list(x, g, w),
        .f = ~ append(.x, getDescriptors(.y)),
        .init = NULL) 
    } else {
      .descriptors
    }
    
    data_summary(
      purrr::map(
        .x = desc,
        .f = function(f) .describe(f, x = x, g = g, w = w, ...))
    )
  }
)

#' @rdname describe
#' @importFrom vctrs field
#' @export

setMethod(
  f          = "describe",
  signature  = c("v_rcensored", "maybeGroup", "maybeWeight", "maybeDescriptor"),
  definition = function(x, g, w, .descriptors, ...){
    
    
    data_summary(
      purrr::map(
        .x = standardDescriptors,
        .f = function(f) .describe(f, x = as_canonical(vctrs::field(x, "time")), ...)
      )
    )
  }
)

#' @rdname describe
#' @export

setMethod(
  f          = "describe",
  signature  = c("data.frame", "maybeGroup", "maybeWeight", "maybeDescriptor"),
  definition = function(x, g, w, .descriptors, ...){
    
    # TODO: add described() method which detects whether a variable has been 
    # previously describe()d using the same arguments. If it has, then simply
    # return the description slot rather than carrying out computations.
    purrr::map_dfr(x, describe)
    # as.data.frame(attr(x, "desc"))
  }
)


#' Create a function to get data from a field in a stype record type vector
#' 
#' @param field_name the name of \code{field} from which extract data
#' @param what an indexing vector (\code{character} or \code{integer}) identifying
#' elements in the \code{data_summary} to extract
#' @param new_names a \code{character} vector of names with which to label the 
#' elements of result calling the returned function
#' @param .after an optional function applied to the result
#' @noRd
get_from_field <- function(field_name, what, new_names = what, .after = identity){
  
  stopifnot(length(what) == length(new_names))
  function(x){
    out <- get_data_summary(vctrs::field(x, field_name))[what]
    .after(setNames(out, new_names))
  }
}

#' Get the summary from a stype variable
#' 
#' @param x the object from which to get the \code{data_summary}
#' @param element either \code{NULL} to get the full \code{data_summary}
#'  or a length 1 \code{character} to select a particular element of the summary
#' @export

setGeneric(
  name = "get_data_summary", 
  def  = function(x, element) standardGeneric("get_data_summary")
)

#' @rdname get_data_summary
#' @export

setMethod(
  f          = "get_data_summary",
  signature  = c("stype", NULL),
  definition = function(x, element){ attr(x, "data_summary") }
)

#' @rdname get_data_summary
#' @export

setMethod(
  f          = "get_data_summary",
  signature  = c("v_rcensored", NULL),
  definition = function(x, element){ 
    Reduce(
      append,
      list(
        get_from_field("time", 
                       c("n", "has_missing", "sum"), 
                       c("n", "has_missing", "person_time"))(x),
        get_from_field("censored", "num_1", "n_censored")(x),
        get_from_field("outcome", "num_1", "n_events")(x),
        get_from_field("censor_reason", "table", 
                       .after = function(z) { list(censor_reasons = z[[1]]) } )(x),
        get_from_field("outcome_reason", "table", 
                       .after = function(z) list(outcome_reasons = z[[1]]) )(x)
      )
    )
    
  }
)

#' @rdname get_data_summary
#' @export

setMethod(
  f          = "get_data_summary",
  signature  = c("stype", "character"),
  definition = function(x, element){ attr(x, "data_summary")[[element]] }
)

#' Check that an object is a stype vector
#' 
#' @param object any \code{R} object
#' @export

is_stype <- function(object) {
  is(object, "stype")
}
