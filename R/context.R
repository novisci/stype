#' stype contexts
#' 
#' @description
#' A context contains study design specific information about a variable.
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

# Context Getters ####

#' Get an object's context or elements thereof
# @name context_get_set
#' @param x a stype object
#' @rdname context_get_set
#' @export
setGeneric("get_context", function(x) standardGeneric("get_context"))

#' @rdname context_get_set
# @aliases get_context,stype,stype-method
#' @export
setMethod("get_context", "stype", function(x){ attr(x, "context") })

#' @rdname context_get_set
#' @export
setGeneric("get_short_label", function(x) standardGeneric("get_short_label"))

#' @template context_get_set
#' @templateVar getorset get
#' @templateVar slot short_label 
#' @templateVar class context
setMethod("get_short_label", "context", function(x) slot(x, "short_label"))

#' @template context_get_set
#' @templateVar getorset get
#' @templateVar slot short_label 
#' @templateVar class stype
setMethod("get_short_label", "stype", function(x) get_short_label(get_context(x)))

#' @rdname context_get_set
#' @export
setGeneric("get_long_label", function(x) standardGeneric("get_long_label"))

#' @template context_get_set
#' @templateVar getorset get
#' @templateVar slot long_label 
#' @templateVar class context
setMethod("get_long_label", "context", function(x) slot(x, "long_label"))

#' @template context_get_set
#' @templateVar getorset get
#' @templateVar slot long_label 
#' @templateVar class stype
setMethod("get_long_label", "stype",function(x) get_long_label(get_context(x)))

#' @rdname context_get_set
#' @export
setGeneric("get_description", function(x) standardGeneric("get_description"))

#' @template context_get_set
#' @templateVar getorset get
#' @templateVar slot description 
#' @templateVar class context
setMethod("get_description", "context", function(x) slot(x, "description"))

#' @template context_get_set
#' @templateVar getorset get
#' @templateVar slot description 
#' @templateVar class stype
setMethod("get_description", "stype", function(x) get_description(get_context(x)))

#' @rdname context_get_set
#' @export
setGeneric("get_derivation", function(x) standardGeneric("get_derivation"))

#' @template context_get_set
#' @templateVar getorset get
#' @templateVar slot derivation 
#' @templateVar class context
setMethod("get_derivation", "context", function(x) slot(x, "derivation"))

#' @template context_get_set
#' @templateVar getorset get
#' @templateVar slot derivation 
#' @templateVar class stype
setMethod("get_derivation", "stype", function(x) get_derivation(get_context(x)))

#' @rdname context_get_set
#' @export
setGeneric("get_purpose", function(x) standardGeneric("get_purpose"))

#' @template context_get_set
#' @templateVar getorset get
#' @templateVar slot purpose 
#' @templateVar class context
setMethod("get_purpose", "context", function(x) slot(x, "purpose"))

#' @template context_get_set
#' @templateVar getorset get
#' @templateVar slot purpose 
#' @templateVar class stype
setMethod("get_purpose", "stype", function(x) get_purpose(get_context(x)))

#' @rdname context_get_set
#' @export
setGeneric("get_security_type", function(x) standardGeneric("get_security_type"))

#' @template context_get_set
#' @templateVar getorset get
#' @templateVar slot security_type 
#' @templateVar class context
setMethod("get_security_type", "context", function(x) slot(x, "security_type"))

#' @template context_get_set
#' @templateVar getorset get
#' @templateVar slot security_type 
#' @templateVar class stype
setMethod("get_security_type", "stype", 
          function(x) get_security_type(get_context(x)))

#' @rdname context_get_set
#' @aliases get_purpose,stype,stype-method
#' @export
setMethod("get_purpose", "stype", function(x){ get_purpose(get_context(x)) })


# TODO: can these functions and their documentation be automatically generated?
# TODO: since these are essentially functors, can purrr::lift be used?

# Context setters ####

#' @rdname context_get_set
#' @aliases set_context
#' @param to what to set the element to
#' @importFrom methods "slot<-"
#' @export
setGeneric("set_context", function(x, to) standardGeneric("set_context"))

#' @rdname context_get_set
#' @aliases set_context,stype,stype-method
#' @export
setMethod("set_context", c("stype", "context"), 
           function(x, to){ attr(x, "context") <- to; x  })

#' @rdname context_get_set
#' @export
setGeneric("set_short_label", function(x, to) standardGeneric("set_short_label"))

#' @template context_get_set
#' @templateVar getorset set
#' @templateVar slot short_label 
#' @templateVar class context
setMethod("set_short_label", c("context", "character"),
          function(x, to) { slot(x, "short_label") <- to; x } )

#' @template context_get_set
#' @templateVar getorset set
#' @templateVar slot short_label 
#' @templateVar class stype
setMethod(
  f = "set_short_label",
  signature  = c("stype", "character"),
  definition = function(x, to) { 
    attr(x, "context") <- set_short_label(attr(x, "context"), to)
    x
   }
)



#' @rdname context_get_set
#' @export
setGeneric("set_long_label", function(x, to) standardGeneric("set_long_label"))

#' @template context_get_set
#' @templateVar getorset set
#' @templateVar slot long_label 
#' @templateVar class context
setMethod("set_long_label", c("context", "character"),
          function(x, to) { slot(x, "long_label") <- to; x } )

#' @template context_get_set
#' @templateVar getorset set
#' @templateVar slot long_label 
#' @templateVar class stype
setMethod(
  f = "set_long_label",
  signature  = c("stype", "character"),
  definition = function(x, to) { 
    attr(x, "context") <- set_long_label(attr(x, "context"), to)
    x
  }
)


#' @rdname context_get_set
#' @export
setGeneric("set_description", function(x, to) standardGeneric("set_description"))

#' @template context_get_set
#' @templateVar getorset set
#' @templateVar slot description 
#' @templateVar class context
setMethod("set_description", c("context", "character"),
          function(x, to) { slot(x, "description") <- to; x } )

#' @template context_get_set
#' @templateVar getorset set
#' @templateVar slot description 
#' @templateVar class stype
setMethod(
  f = "set_description",
  signature  = c("stype", "character"),
  definition = function(x, to) { 
    attr(x, "context") <- set_description(attr(x, "context"), to)
    x
  }
)


#' @rdname context_get_set
#' @export
setGeneric("set_derivation", function(x, to) standardGeneric("set_derivation"))

#' @template context_get_set
#' @templateVar getorset set
#' @templateVar slot derivation 
#' @templateVar class context
setMethod("set_derivation", c("context", "character"),
          function(x, to) { slot(x, "derivation") <- to; x } )

#' @template context_get_set
#' @templateVar getorset set
#' @templateVar slot derivation 
#' @templateVar class stype
setMethod(
  f = "set_derivation",
  signature  = c("stype", "character"),
  definition = function(x, to) { 
    attr(x, "context") <- set_derivation(attr(x, "context"), to)
    x
  }
)


#' @rdname context_get_set
#' @export
setGeneric("set_purpose", function(x, to) standardGeneric("set_purpose"))

#' @template context_get_set
#' @templateVar getorset set
#' @templateVar slot purpose 
#' @templateVar class context
setMethod("set_purpose", c("context", "purpose"),
          function(x, to) { slot(x, "purpose") <- to; x } )

#' @template context_get_set
#' @templateVar getorset set
#' @templateVar slot purpose 
#' @templateVar class stype
setMethod(
  f = "set_purpose",
  signature  = c("stype", "purpose"),
  definition = function(x, to) { 
    attr(x, "context") <- set_purpose(attr(x, "context"), to)
    x
  }
)

#' @rdname context_get_set
#' @export
setGeneric("set_security_type", function(x, to) standardGeneric("set_security_type"))

#' @template context_get_set
#' @templateVar getorset set
#' @templateVar slot security_type 
#' @templateVar class context
setMethod("set_security_type", c("context", "character"),
          function(x, to) { slot(x, "security_type") <- to; x } )

#' @template context_get_set
#' @templateVar getorset set
#' @templateVar slot security_type 
#' @templateVar class stype
setMethod(
  f = "set_security_type",
  signature  = c("stype", "character"),
  definition = function(x, to) { 
    attr(x, "context") <- set_security_type(attr(x, "context"), to)
    x
  }
)

# Context predicates ####
#' @rdname is_study_role
#' @aliases is_study_role,context,context-method
#' @export 
setMethod("is_study_role", "context", function(object, what){ 
  is_study_role(get_purpose(object), what)
})

#' @rdname is_study_role
#' @aliases is_study_role,stype,stype-method
#' @export 
setMethod("is_study_role", "stype", function(object, what){ 
  is_study_role(get_purpose(object), what)
})

#' @rdname is_study_role
#' @aliases is_identifier,context,context-method
#' @export 
setMethod("is_identifier", "context", function(object){ 
  is_identifier(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_identifier,stype,stype-method
#' @export 
setMethod("is_identifier", "stype", function(object){ 
  is_identifier(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_index,context,context-method
#' @export 
setMethod("is_index", "context", function(object){ 
  is_index(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_index,stype,stype-method
#' @export 
setMethod("is_index", "stype", function(object){ 
  is_index(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_outcome,context,context-method
#' @export 
setMethod("is_outcome", "context", function(object){ 
  is_outcome(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_outcome,stype,stype-method
#' @export 
setMethod("is_outcome", "stype", function(object){ 
  is_outcome(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_censoring,context,context-method
#' @export 
setMethod("is_censoring", "context", function(object){ 
  is_censoring(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_censoring,stype,stype-method
#' @export 
setMethod("is_censoring", "stype", function(object){ 
  is_censoring(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_competing,context,context-method
#' @export 
setMethod("is_competing", "context", function(object){ 
  is_competing(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_competing,stype,stype-method
#' @export 
setMethod("is_competing", "stype", function(object){ 
  is_competing(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_exposure,context,context-method
#' @export 
setMethod("is_exposure", "context", function(object){ 
  is_exposure(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_exposure,stype,stype-method
#' @export 
setMethod("is_exposure", "stype", function(object){ 
  is_exposure(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_covariate,context,context-method
#' @export 
setMethod("is_covariate", "context", function(object){ 
  is_covariate(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_covariate,stype,stype-method
#' @export 
setMethod("is_covariate", "stype", function(object){ 
  is_covariate(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_weight,context,context-method
#' @export 
setMethod("is_weight", "context", function(object){ 
  is_weight(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_weight,stype,stype-method
#' @export 
setMethod("is_weight", "stype", function(object){ 
  is_weight(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_intermediate,context,context-method
#' @export 
setMethod("is_intermediate", "context", function(object){ 
  is_intermediate(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_intermediate,stype,stype-method
#' @export 
setMethod("is_intermediate", "stype", function(object){ 
  is_intermediate(get_context(object))
})

#' @rdname is_study_role
#' @aliases is_other,context,context-method
#' @export 
setMethod("is_other", "context", function(object){ 
  is_other(get_purpose(object))
})

#' @rdname is_study_role
#' @aliases is_other,stype,stype-method
#' @export 
setMethod("is_other", "stype", function(object){ 
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
    msg = "All context elements must be equal in order to combine stypes."
  )
  
  pslts <- methods::slotNames("purpose")
  # Check purpose slots
  assertthat::assert_that(
      all(purrr::map_lgl(
        .x = pslts,
        .f = ~ methods::slot(get_purpose(x), .x) == methods::slot(get_purpose(y), .x)
      )),
    msg = "All purpose elements must be equal in order to combine stypes."
  )
}