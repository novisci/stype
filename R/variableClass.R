#------------------------------------------------------------------------------#
# Defines a class "variable", which basically adds metadata to vectors of 
# certain types.
# TODO: should this class definition live in a separate package?
#------------------------------------------------------------------------------#

#' The variable R object
#'
#' Extends R's \code{\link{vector-class}} to provide objects with additional information. 
#' The vector object must be one of the following basic classes: \code{"numeric", "double", 
#' "integer", "logical", "character",}, or \code{\link{factor}}, or \code{\link{Date}}.
#'  A variable object can used as if it was a vector.
#' 
#' @slot name machine readable name
#' @slot short_label a short label
#' @slot long_label a longer label
#' @slot description a \linkS4class{description} object containing summary statistics
#' @importFrom purrr walk
#' @importFrom assertthat validate_that
#' @importFrom methods slot new callNextMethod
#' @export variable

variable <- setClass(
  "variable", 
  slots    = c(
    # TODO: use the string class from usefulClasses?
    "name"          = "character",
    "short_label"   = "character",
    "long_label"    = "character",
    "description"   = "description"),
  contains = "vector"
)

# Initialize a variable object
# @export 
setMethod(
  f          = "initialize",
  signature  = "variable",
  definition = function(.Object, ..., descriptor = descriptor(.Object), g = NULL, w = NULL){

    # TODO: there is potential for conflicting arguments if the describe function's
    # ... takes arguments with the same name as any slots in a variable.
    # One could remove any arguments with the same name as any slotNames("variable")
    # (and give a warning)
    .Object <- callNextMethod(.Object, ...)
    
    ## Hijack the user's ability to (incorrectly) set slots
    #TODO: enforce that the first element of dots is the data which is already
    # added to .Object by the previous call
    dots <- list(...)[-1]
    desc <- if(missing(descriptor)) NULL else descriptor
    dots[["description"]]  <- describe(.Object, g, w, descriptor = desc,...)
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
