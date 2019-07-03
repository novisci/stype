#' A context contains study design specific information about a variable
#' 
#' @name context
#' @slot short_label a short_label
#' @slot long_label a long_label
#' @slot description a description
#' @slot derivation a derivation
#' @slot purpose a purpose
#' @slot security_type security information
#' @importFrom purrr walk
#' @importFrom methods slot slotNames new
#' @importFrom assertthat validate_that
#' @export context

context <- setClass(
  "context",
  slots = c(
    short_label = "character",
    long_label  = "character",
    description = "character",
    derivation  = "character",
    purpose     = "purpose",
    security_type = "character"
  ),
  prototype = methods::prototype(
    short_label = "",
    long_label  = "",
    description = "",
    derivation  = "",
    purpose     = purpose(),
    security_type = ""
    
  )
)

setValidity(
  "context",
  method = function(object){
    
    ctxt_slots <- methods::slotNames("context")
    purrr::walk(
      .x = ctxt_slots[!(ctxt_slots %in% c("purpose"))] ,
      .f = ~ assertthat::validate_that(
        length(methods::slot(object, .x)) == 1,
        msg = sprintf("The %s slot must have length 1.", .x))
    )
    
    TRUE
  }
)



#' Get an object's context
#' @name get_context
#' @param x a described object
#' @export
setGeneric("get_context", function(x) standardGeneric("get_context"))

#' @rdname get_context
#' @aliases get_context,described,described-method
#' @export
setMethod("get_context", "described", function(x){ attr(x, "context") })

#' Get an object's purpose
#' @name get_purpose
#' @param x a described object
#' @export
setGeneric("get_purpose", function(x) standardGeneric("get_purpose"))

#' @rdname get_purpose
#' @aliases get_purpose,context,context-method
#' @export
setMethod("get_purpose", "context", function(x){ slot(x, "purpose") })

#' @rdname get_purpose
#' @aliases get_purpose,described,described-method
#' @export
setMethod("get_purpose", "described", function(x){ get_purpose(get_context(x)) })

# TODO: can these functions and their documentation be automatically generated?
# TODO: since these are essentially functors, can purrr::lift be used?

#' @rdname is_study_role
#' @aliases is_study_role,context,context-method
#' @export 
setMethod("is_study_role", "context", function(object, what){ 
  is_study_role(get_purpose(object), what)
})

#' @rdname is_study_role
#' @aliases is_study_role,described,described-method
#' @export 
setMethod("is_study_role", "described", function(object, what){ 
  is_study_role(get_purpose(object), what)
})

#' @rdname is_study_role
#' @aliases is_identifier,context,context-method
#' @export 
setMethod("is_identifier", "context", function(object){ 
  is_identifier(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_identifier,described,described-method
#' @export 
setMethod("is_identifier", "described", function(object){ 
  is_identifier(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_index,context,context-method
#' @export 
setMethod("is_index", "context", function(object){ 
  is_index(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_index,described,described-method
#' @export 
setMethod("is_index", "described", function(object){ 
  is_index(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_outcome,context,context-method
#' @export 
setMethod("is_outcome", "context", function(object){ 
  is_outcome(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_outcome,described,described-method
#' @export 
setMethod("is_outcome", "described", function(object){ 
  is_outcome(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_censoring,context,context-method
#' @export 
setMethod("is_censoring", "context", function(object){ 
  is_censoring(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_censoring,described,described-method
#' @export 
setMethod("is_censoring", "described", function(object){ 
  is_censoring(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_competing,context,context-method
#' @export 
setMethod("is_competing", "context", function(object){ 
  is_competing(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_competing,described,described-method
#' @export 
setMethod("is_competing", "described", function(object){ 
  is_competing(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_exposure,context,context-method
#' @export 
setMethod("is_exposure", "context", function(object){ 
  is_exposure(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_exposure,described,described-method
#' @export 
setMethod("is_exposure", "described", function(object){ 
  is_exposure(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_covariate,context,context-method
#' @export 
setMethod("is_covariate", "context", function(object){ 
  is_covariate(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_covariate,described,described-method
#' @export 
setMethod("is_covariate", "described", function(object){ 
  is_covariate(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_weight,context,context-method
#' @export 
setMethod("is_weight", "context", function(object){ 
  is_weight(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_weight,described,described-method
#' @export 
setMethod("is_weight", "described", function(object){ 
  is_weight(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_intermediate,context,context-method
#' @export 
setMethod("is_intermediate", "context", function(object){ 
  is_intermediate(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_intermediate,described,described-method
#' @export 
setMethod("is_intermediate", "described", function(object){ 
  is_intermediate(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_other,context,context-method
#' @export 
setMethod("is_other", "context", function(object){ 
  is_other(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_other,described,described-method
#' @export 
setMethod("is_other", "described", function(object){ 
  is_other(get_context(object))
})

setGeneric("is_empty", function(x) standardGeneric("is_empty"))
setMethod("is_empty", "character", function(x)  length(x) == 0 | nchar(x) == 0)
setMethod("is_empty", "purpose", function(x) is_empty(methods::slot(x, "study_role")))
setMethod("is_empty", "context", function(x){
  empties <- purrr::map_lgl(
    .x = methods::slotNames("context"),
    .f = ~ is_empty(methods::slot(x, .x)))
  all(empties)
})



# Check whether two contexts are the same
compare_contexts <- function(x, y){
  
  cslts <- methods::slotNames("context")
  
  # Check non-purpose slots
  assertthat::assert_that(
    all(purrr::map_lgl(
      .x = cslts[!(cslts == "purpose")],
      .f = ~ methods::slot(get_context(x), .x) == methods::slot(get_context(y), .x)
    )),
    msg = "All context elements must equal in order to combine"
  )
  
  pslts <- methods::slotNames("purpose")
  # Check purpose slots
  assertthat::assert_that(
      all(purrr::map_lgl(
        .x = pslts,
        .f = ~ methods::slot(get_purpose(x), .x) == methods::slot(get_purpose(y), .x)
      )),
    msg = "All purpose elements must equal in order to combine"
  )
}