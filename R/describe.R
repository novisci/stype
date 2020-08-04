#------------------------------------------------------------------------------#
# Defines the following
#
# - useful class unions 
# - getDescriptors: a method that gets descriptors
# - .describe: the internal method for applying a descriptor
# - describe: applies a descriptor to a variable

globalVariables(c("view", "set"))
# Classes used internally to define methods for signatures with possibly missing
# arguments.

#' Stype classes
#' 
#' S4 classes uses in defining methods.
#' @name stype_classes
NULL

#' @rdname stype_classes
#' @export
setClassUnion("groupVar",        c("character", "factor"))

#' @rdname stype_classes
#' @export
setClassUnion("weightVar",       c("numeric"))

#' @rdname stype_classes
#' @export
setClassUnion("categorical",     c("factor", "ordered"))

#' @rdname stype_classes
#' @export
setClassUnion("Missing",         c("missing", "NULL"))

#' @rdname stype_classes
#' @export
setClassUnion("maybeGroup",      c("Missing", "groupVar"))

#' @rdname stype_classes
#' @export
setClassUnion("maybeWeight",     c("Missing", "weightVar"))

#' @rdname stype_classes
#' @export
setClassUnion("maybeDescriptor", c("missing", "NULL", "list"))

#' @rdname stype_classes
#' @export
setClassUnion("describable",     c("integer", "logical", "numeric", "factor",
                                   "ordered", "character", "v_rcensored"))

#' @rdname stype_classes
#' @export
setClassUnion("simpleStype",     c("v_count", "v_binary", "v_continuous", 
                                   "v_continuous_nonneg", "v_event_time",
                                   "v_nominal", "v_ordered", "v_character"))

#' @rdname stype_classes
#' @export
setClassUnion("complexStype",    c("v_rcensored"))

#' @rdname stype_classes
#' @export
setClassUnion("stype",           c("v_count", "v_binary", "v_continuous",
                                   "v_continuous_nonneg", "v_event_time",
                                   "v_nominal", "v_ordered", "v_character",
                                   "v_rcensored"))

#' Descriptors
#' @name descriptors 
#' @description 
#' Lists of functions to used to \code{\link{describe}} a \code{describeable}
#' vector to create a \code{\link{data_summary}} object. A \code{descriptor} is
#' a function of \code{x} (and optionally \code{g} (a grouping vector and/or 
#' \code{w} a numeric weight vector)).
#' 
#' \code{describable} vectors include: integer, logical, numeric, factor, 
#' ordered, and v_rcensored.
#' 
#' @importFrom stats IQR median sd quantile var cov weighted.mean
#' @keywords internal
NULL

#' @rdname descriptors 
standardDescriptors <- list(
  n            = function(x, ...) length(x),
  has_missing  = function(x, ...) anyNA(x),
  n_nonmissing = function(x, ...) sum(!is.na(x)),
  n_missing    = function(x, ...) sum(is.na(x)),
  proportion_missing = function(x, ...) mean(is.na(x)),
  is_constant  = function(x, ...) all(x[1] == x)
)

#' @rdname descriptors
logicalDescriptors <- list(
  num_0      = function(x, ...) sum(!x, na.rm = TRUE),
  num_1      = function(x, ...) sum(x, na.rm = TRUE),
  proportion = function(x, ...) mean(x, na.rm = TRUE),
  variance   = function(x, ...) var(x, na.rm = TRUE)
)

#' @rdname descriptors
wlogicalDescriptors <- list(
  weighted_proportion = function(x, w, ...) { 
    stats::weighted.mean(x = x, w = w, ...)
  }
)

#' @rdname descriptors
categoricalDescriptors <- list(
  table  = function(x, ...) table(x, useNA = "always"),
  ptable = function(x, ...) prop.table(table(x, useNA = "always")),
  levels = function(x, ...) levels(x)
)

#' @rdname descriptors
characterDescriptors <- list(
  n_unique = function(x, ...) length(unique(x)),
  max_char = function(x, ...) if(length(x) == 0) 0 else max(nchar(x)),
  min_char = function(x, ...) if(length(x) == 0) 0 else min(nchar(x))
)

#' @rdname descriptors
numericDescriptors <- list(
  sum    = function(x, ...) sum(x, na.rm = TRUE),
  mean   = function(x, ...) mean(x, na.rm = TRUE),
  variance = function(x, ...) var(x, na.rm = TRUE),
  median = function(x, ...) median(x, na.rm = TRUE),
  iqr    = function(x, ...) IQR(x, na.rm = TRUE),
  min    = function(x, ...) { 
    if (length(x) == 0 || all(is.na(x))) NA_real_ else min(x, na.rm = TRUE) 
  },
  max    = function(x, ...) { 
    if (length(x) == 0 || all(is.na(x))) NA_real_ else max(x, na.rm = TRUE) 
  },
  qtiles = function(x, ...) { 
    quantile(
      x, 
      probs = c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99), 
      na.rm = TRUE)
  }
)

#' @rdname descriptors
wnumericDescriptors <- list(
  weighted_mean = function(x, w, ...){ stats::weighted.mean(x = x, w = w, ...) }
)

#' @rdname descriptors
groupedDescriptors <- list(
  smd = function(x, g, w = NULL, ...) smd::smd(x, g, w, na.rm = TRUE)
)

#' @rdname descriptors
rcensoredDescriptors <- list(
  time_info = function(x, ...) get_persontime(x),
  outcome_info = function(x, ...) get_outcomeinfo(x),
  censor_info  = function(x, ...) get_censorinfo(x),
  eair = function(x, ...) eair(x)
)

#' Get person-time info for a data_summary
#' @param x a \code{\link{v_rcensored}} vector
#' @keywords internal
get_persontime <- function(x, ...){
  get_from_field("time", "sum", "person_time")(x)
}

#' Get outcome info for a data_summary
#' @param x a \code{\link{v_rcensored}} vector
#' @keywords internal
get_outcomeinfo <- function(x, ...){
  vctrs::vec_c(
    get_from_field("outcome", "num_1", "n_events")(x),
    get_from_field("outcome_reason", "table", 
                   .after = function(z) list(outcome_reasons = z[[1]]) )(x)
  )
}

#' Get censoring info for a data_summary
#' @param x a \code{\link{v_rcensored}} vector
#' @keywords internal
get_censorinfo <- function(x, ...){
  vctrs::vec_c(
    get_from_field("censored", "num_1", "n_censored")(x),
    get_from_field("censor_reason", "table", 
                   .after = function(z) { list(censor_reasons = z[[1]]) } )(x)
  )
}
#' Exposure-Adjusted Incidence Rate
#' 
#' Computes the exposure-adjusted incidence rate and variance using He et al. 
#' (2015).
#' 
#' @param x a \code{\link{v_rcensored}} vector
#' @references 
#'    He X, Chen L, Lei L, Xia HA, Lee MLT (2015) A Simple Method for
#'     Estimating Confidence Intervals for Exposure Adjusted Incidence Rate and
#'     Its Applications to Clinical Trials. J Biom Biostat 6: 238.
#'     doi:10.4172/2155-6180.1000238
#' @return a \code{list} containing:
#'    \itemize{
#'      \item eair the point estimate
#'      \item eair_variance the variance estimate
#'    }
#' @export
eair <- function(x){
  outc <- vctrs::field(x, "outcome")
  time <- vctrs::field(x, "time")
  
  nevents <- get_data_summary(outc, "num_1")
  pt <- get_data_summary(time, "sum")
  # proportion of subjects that have event prior to censor or end of followup
  a_hat   <- get_data_summary(outc, "proportion")
  a_sigma <- get_data_summary(outc, "variance")
  
  # mean followup time
  b_hat   <- get_data_summary(time, "mean")
  b_sigma <- get_data_summary(time, "variance")
  
  cov_ab <- cov(outc, time)
  
  sigma <- matrix(c(a_sigma, cov_ab, cov_ab, b_sigma), ncol = 2, byrow = TRUE)
  L <- c(1 , -a_hat/b_hat)
  
  list(
    eair          = nevents/pt,
    eair_variance = drop((1/(b_hat^2)) * t(L) %*% sigma %*% L)
  )
}

#' getDescriptors
#'
#' Returns a list of functions to be applied to a variable.
#' 
#' @param x a vector of data
#' @param g a vector of groupings
#' @param w a vector of weights
# @export
#' @keywords internal
setGeneric("getDescriptors", function(x, g, w) standardGeneric("getDescriptors"))

#' @rdname getDescriptors
#' @export
setMethod(
  f          = "getDescriptors",
  signature  = c("logical", "maybeGroup", "maybeWeight"),
  definition = function(x, g, w){
    vctrs::vec_c(
      standardDescriptors,
      logicalDescriptors,
      `if`(is(g, "groupVar"), groupedDescriptors, NULL),
      `if`(is(w, "weightVar"), wlogicalDescriptors, NULL)
    )
  }
)

#' @rdname getDescriptors
#' @export
setMethod(
  f          = "getDescriptors",
  signature  = c("categorical", "maybeGroup", "maybeWeight"),
  definition = function(x, g, w){
    vctrs::vec_c(
      standardDescriptors,
      categoricalDescriptors,
      `if`(is(g, "groupVar"), groupedDescriptors, NULL)
    )
  }
)

#' @rdname getDescriptors
#' @export
setMethod(
  f          = "getDescriptors",
  signature  = c("character", "maybeGroup", "maybeWeight"),
  definition = function(x, g, w){
    vctrs::vec_c(
      standardDescriptors,
      characterDescriptors,
      `if`(is(g, "groupVar"), groupedDescriptors, NULL)
    )
  }
)

#' @rdname getDescriptors
#' @export
setMethod(
  f          = "getDescriptors",
  signature  = c("numeric", "maybeGroup", "maybeWeight"),
  definition = function(x, g, w){
    vctrs::vec_c(
      standardDescriptors,
      numericDescriptors,
      `if`(is(g, "groupVar"), groupedDescriptors, NULL),
      `if`(is(w, "weightVar"), wnumericDescriptors, NULL)
    )
  }
)

#' @rdname getDescriptors
#' @export
setMethod(
  f          = "getDescriptors",
  signature  = c("v_rcensored", "maybeGroup", "maybeWeight"),
  definition = function(x, g, w){
    vctrs::vec_c(
      standardDescriptors,
      rcensoredDescriptors
    )
  }
)

# Describe a variable (internal method)
# 
# The internal method for applying a descriptive function on x and, optionally, 
# g and/or w. The methods define the signature patterns for applying the 
# functions.

setGeneric(".describe", function(f, x, g, w, ...) standardGeneric(".describe"))

.describeMethods <- list(
  list(
    sig = c("function", "describable", "Missing", "Missing"),
    bod = quote(f(x, ...))
  ),
  list(
    sig = c("function", "describable", "groupVar", "Missing"),
    bod = quote(f(x = x, g = g, ...))
  ),
  list(
    sig = c("function", "describable", "Missing", "numeric"),
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

    descriptors <- `if`(
      missing(.descriptors) || methods::is(.descriptors, "Missing"),
      getDescriptors(x, g, w),
      vctrs::vec_c(getDescriptors(x, g, w), .descriptors)
    )
    
    data_summary(
      purrr::map(
        .x = descriptors,
        .f = function(f) .describe(f, x = x, g = g, w = w, ...))
    )
  }
)

#' @rdname describe
#' @export
setMethod(
  f          = "describe",
  signature  = c("simpleStype", "maybeGroup", "maybeWeight", "maybeDescriptor"),
  definition = function(x, g, w, .descriptors, ...){
    cl <- match.call()
    cl <- lapply(as.list(cl), eval, sys.parent())
    cl$x <- as_canonical(cl$x)
    eval(as.call(cl))
  }
)

#' @rdname describe
#' @importFrom vctrs field
#' @importFrom purrr flatten
#' @export
setMethod(
  f          = "describe",
  signature  = c("v_rcensored", "maybeGroup", "maybeWeight", "maybeDescriptor"),
  definition = function(x, g, w, .descriptors, ...){
    
    standdesc <- 
      purrr::map(
        .x = standardDescriptors,
        .f = function(f) .describe(f, x = as_canonical(vctrs::field(x, "time")), ...)
      )
    
    rcensoredDescriptors <- `if`(
      missing(.descriptors) || methods::is(.descriptors, "Missing"),
      rcensoredDescriptors,
      vctrs::vec_c(rcensoredDescriptors, .descriptors)
    )
    
    rcensdesc <-
      purrr::flatten(purrr::map(
      .x = rcensoredDescriptors,
      .f = function(f) .describe(f, x = x, g = g, w = w, ...)
    ))
    
    data_summary(
      vctrs::vec_c(
        standdesc,
        rcensdesc
      )
    )
  }
)

#' (Re)\code{describe} a \code{stype} by weighting the vector.
#' @rdname describe
#' @export
setGeneric(
  name = "weight", 
  def  = function(x, w, .descriptors) standardGeneric("weight")
)

#' @rdname describe
#' @export
setMethod(
  f          = "weight",
  signature  = c("stype", "weightVar", "maybeDescriptor"),
  definition = function(x, w, .descriptors){
    cl <- swap_function(match.call(), describe)
    set(x, data_summmary_l, eval(cl, sys.parent()))
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
  signature  = c("stype", "character"),
  definition = function(x, element){ attr(x, "data_summary")[[element]] }
)

#' Get the internal name from a stype variable
#' 
#' @param x the object from which to get the \code{internal_name} attribute
#' @export
setGeneric(
  name = "get_internal_name", 
  def  = function(x) standardGeneric("get_internal_name")
)

#' @rdname get_internal_name
#' @export
setMethod(
  f          = "get_internal_name",
  signature  = c("stype"),
  definition = function(x){ attr(x, "internal_name") }
)

#' Check that an object is a stype vector
#' 
#' @param object any \code{R} object
#' @export
is_stype <- function(object) {
  is(object, "stype")
}
