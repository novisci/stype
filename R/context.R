#' A context contains study design specific information about a variable
#' 
#' @name context
#' @slot name a name
#' @slot short_label a short_label
#' @slot long_label a long_label
#' @slot purpose a purpose
#' @importFrom purrr walk
#' @importFrom methods slot slotNames new
#' @importFrom assertthat validate_that
#' @export context

context <- setClass(
  "context",
  slots = c(
    name        = "character",
    short_label = "character",
    long_label  = "character",
    purpose     = "character"
  ),
  prototype = methods::prototype(
    name        = "",
    short_label = "",
    long_label  = "",
    purpose     = ""
  )
)

validPurposes <- c("outcome", "exposure", "covariate", "other")

setValidity(
  "context",
  method = function(object){
    
    purrr::walk(
      .x = methods::slotNames("context"),
      .f = ~ assertthat::validate_that(
        length(methods::slot(object, .x)) == 1,
        msg = sprintf("The %s slot must have length 1.", .x)
      )
    )
    
    assertthat::validate_that(
      methods::slot(object, "purpose") %in% validPurposes,
      msg = sprintf("purpose slot must be on of %s.", paste(validPurposes, collapse = ", "))
    )
    
  }
)

#' Check a context
#' 
#' @name is_purpose
#' @param context a context
#' @param x what to check
#' @export
setGeneric("is_purpose", function(context, x) standardGeneric("is_purpose"))

#' @rdname is_purpose
#' @aliases is_purpose,context,context-method
#' @export

setMethod(
  "is_purpose",
  "context",
  function(context, x){
    purp <- methods::slot(context, "purpose") 
    if(length(purp) == 0) {
      FALSE
    } else {
      purp == x
    }
  }
)

#' @rdname is_purpose
#' @aliases is_purpose,NULL,context-method
#' @export
setMethod("is_purpose", "NULL", function(context, x){ FALSE } )

#' Is this vector an outcome?
#' @name is_outcome
#' @inheritParams is_purpose
#' @export
setGeneric("is_outcome", function(x) standardGeneric("is_outcome"))

#' @rdname is_outcome
#' @aliases is_outcome,context,context-method
#' @export
setMethod("is_outcome", "context", function(x){ is_purpose(x, "outcome") })

#' @rdname is_outcome
#' @aliases is_outcome,described,context-method
#' @export 
setMethod("is_outcome", "described", function(x){ is_outcome(attr(x, "context")) })

# @export
# setMethod("is_outcome", "ANY", function(x){ is_outcome(attr(x, "context")) })

#' Is this vector an exposure?
#' @name is_exposure
#' @inheritParams is_purpose
#' @export
setGeneric("is_exposure", function(x) standardGeneric("is_exposure"))

#' @rdname is_exposure
#' @aliases is_exposure,context,context-method
#' @export
setMethod("is_exposure", "context", function(x){ is_purpose(x, "exposure") })

#' @rdname is_exposure
#' @aliases is_exposure,described,context-method
#' @export
setMethod("is_exposure", "described", function(x){ is_exposure(attr(x, "context")) })

#' Is this vector a covariate?
#' @name is_covariate
#' @inheritParams is_purpose
#' @export
setGeneric("is_covariate", function(x) standardGeneric("is_covariate"))

#' @rdname is_covariate
#' @aliases is_covarite,context,context-method
#' @export
setMethod("is_covariate", "context", function(x){ is_purpose(x, "covariate") })

#' @rdname is_covariate
#' @aliases is_covarite,described,context-method
#' @export
setMethod("is_covariate", "described", function(x){ is_covariate(attr(x, "context")) })

#' Get an object's context
#' @name get_context
#' @param x a context
#' @export
setGeneric("get_context", function(x) standardGeneric("get_context"))

#' @rdname get_context
#' @aliases get_context,described,context-method
#' @export
setMethod("get_context", "described", function(x){ attr(x, "context") })


# Test whether a context is empty
is_empty <- function(context){
  
  # browser()
  empties <- purrr::map_lgl(
    .x = methods::slotNames("context"),
    .f = ~ {
      slt <- methods::slot(context, .x)
      length(slt) == 0 | nchar(slt) == 0
    }
  )
  
  all(empties)
}

# Check whether two contexts are the same
compare_contexts <- function(x, y){
  assertthat::assert_that(
    all(purrr::map_lgl(
      .x = methods::slotNames("context"),
      .f = ~ methods::slot(get_context(x), .x) == methods::slot(get_context(y), .x)
    )),
    msg = "All context elements must equal in order to combine"
  )
}