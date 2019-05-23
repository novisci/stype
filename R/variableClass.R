#------------------------------------------------------------------------------#
# Defines a class "variable", which basically adds metadata to vectors of 
# certain types.
# TODO: should this class definition live in a separate package?
# TODO: add support for factors, dates, and times
#------------------------------------------------------------------------------#

#' The variable R object
#'
#' Extends R's \code{\link{vector-class}} to provide objects with additional information. 
#' The vector object must be one of the following basic classes: \code{"numeric", 
#' "integer", "logical", "character"}
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
  definition = function(.Object, ..., .descriptors = NULL, g = NULL, w = NULL){

    # TODO: there is potential for conflicting arguments if the describe function's
    # ... takes arguments with the same name as any slots in a variable.
    # One could remove any arguments with the same name as any slotNames("variable")
    # (and give a warning)
    
    .Object@.Data <- list(...)[[1L]]
    
    ## Hijack the user's ability to (incorrectly) set slots
    #TODO: enforce that the first element of dots is the data which is already
    # added to .Object by the previous call
    dots <- list(...)[-1]
    desc <- append(getDescriptors(.Object), .descriptors)
    .Object@description  <- describe(.Object, g = g, w = w, .descriptors = desc)
    callNextMethod()
    # validObject(.Object) 
    # return(.Object)
    # do.call("callNextMethod", args = append(list(.Object), dots))
  })

# Set the validity of of variable object
setValidity(
  Class  = "variable",
  method = function(object){
    valid_classes <- c("numeric", "double", "integer", "logical")
                       # "factor", "character", "Date")
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
          msg = sprintf("\n%s must be length 1.", x)
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


#' Glance at a variable
#' 
#' @param object an object
#' @importFrom tibble as_tibble
#' @export

setGeneric(
  name = "glance", 
  def  = function(object) standardGeneric("glance")
)

#' @rdname glance
#' @export

setMethod(
  f          = "glance",
  signature  = "variable",
  definition = function(object){
    tibble::tibble(!!! object@description)
  }
)
