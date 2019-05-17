#------------------------------------------------------------------------------#
# Defines a class "variable", which basically adds metadata to vectors of 
# certain types.
# TODO: should this class definition live in a separate package?
#------------------------------------------------------------------------------#

#' The variable R object
#'
#' Extends R's \link{\code{vector-class}} to provide objects with additional information. 
#' The vector object must be one of the following basic classes: \code{"numeric", "double", 
#' "integer", "logical", "character",}, or \link{\code{"factor"}}, or \link{\code{"Date"}}.
#'  A variable object can used as if it was a vector.
#' 
#' @slot name machine readable name
#' @slot short_label a short label
#' @slot long_label a longer label
#' @slot has_missing an indicator of whether a variable has missing values
#' @slot internal_type the internal 
#' @slot description a \linkS4class{description} object containing summary statistics
#' @importFrom purrr walk
#' @importFrom assertthat validate_that
#' @importFrom methods slot
#' @export variable

variable <- setClass(
  "variable", 
  slots    = c(
    # TODO: use the string class from usefulClasses?
    "name"          = "character",
    "short_label"   = "character",
    "long_label"    = "character",
    "has_missing"   = "logical",   # TODO: put this in description?
    "internal_type" = "character", # TODO: do we need this?
    "description"   = "description"),
  contains = "vector"
)

# Initialize a variable object
setMethod(
  f          = "initialize",
  signature  = "variable",
  definition = function(.Object, ...){

    # Hijack the user's ability to (incorrectly) set has_missing
    # e.g. variable(c(TRUE, NA), has_missing = FALSE)
    dots <- list(...)
    dots[["has_missing"]]   <- anyNA(..1)
    dots[["internal_type"]] <- typeof(..1)
    
    do.call("callNextMethod", args = append(list(.Object), dots))
  })

# Set the validity of of variable object
setValidity(
  Class  = "variable",
  method = function(object){
    
    valid_classes <- c("numeric", "double", "integer", "logical",
                       "factor", "character", "Date")
    data_class    <- class(methods::getDataPart(object))
    
    assertthat::validate_that(
      data_class %in% valid_classes,
      msg = sprintf("\nThe data of a variable must be one of %s, not %s.",
                    paste(valid_classes, collapse = ", "),
                    data_class)
    )
    
    purrr::walk(
      .x = c("name", "short_label"),
      .f = function(x){
        assertthat::validate_that(
          length(methods::slot(object, x)) == 1,
          msg = "short_label must be length 1"
        )
      }
    )
    
    TRUE
  }
)

#' @rdname show-methods
#' @aliases show,variable,variable-method
#' @export

setMethod(
  f          = "show",
  signature  = "variable",
  definition = function(object){
    cat(object@short_label,"\n", object@.Data)
  }
)
