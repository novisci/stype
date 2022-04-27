#' Purpose object
#' 
#' @slot study_role the study role
#' @slot tags a character vector of tags
#' @importFrom purrr walk
#' @importFrom methods slot slotNames new
#' @importFrom assertthat validate_that
#' @export purpose
purpose <- setClass(
  "purpose",
  slots = c(
    "study_role" = "character",
    "tags"       = "character"
  )
)

valid_roles <- c("identifier", "index",
                 "outcome", "censoring", "competing",
                 "exposure", "covariate", "weight", 
                 "intermediate", "other")

setValidity(
  "purpose",
  function(object){
    assertthat::validate_that(
      all(methods::slot(object, "study_role") %in% valid_roles),
      msg = sprintf("study roles must be one of %s.", 
                    paste(valid_roles, collapse = ", "))
    )
  }
)

#' Check a study role
#' 
#' @name is_study_role
#' @param object an object
#' @param what what to role to look for
#' @export
setGeneric("is_study_role", function(object, what) standardGeneric("is_study_role"))

#' @rdname is_study_role
#' @param object an object
#' @param what what to role to look for
#' @aliases is_study_role,purpose,purpose-method
#' @export
setMethod(
  "is_study_role",
  "purpose",
  function(object, what){
    purps <- methods::slot(object, "study_role") 
    if(length(purps) == 0) {
      FALSE
    } else {
      what %in% purps
    }
  }
)

#' @rdname is_study_role
#' @aliases is_study_role,NULL,NULL-method
#' @export
setMethod("is_study_role", "NULL", function(object, what){ FALSE } )

#' @rdname is_study_role
#' @export
setGeneric("is_identifier", function(object) standardGeneric("is_identifier"))

#' @rdname is_study_role
#' @aliases is_identifer,purpose,purpose-method
#' @export
setMethod("is_identifier", "purpose", function(object){ is_study_role(object, "identifier")})

#' @rdname is_study_role
#' @export
setGeneric("is_index", function(object) standardGeneric("is_index"))

#' @rdname is_study_role
#' @aliases is_index,purpose,purpose-method
#' @export
setMethod("is_index", "purpose", function(object){ is_study_role(object, "index")})

#' @rdname is_study_role
#' @export
setGeneric("is_outcome", function(object) standardGeneric("is_outcome"))

#' @rdname is_study_role
#' @aliases is_outcome,purpose,purpose-method
#' @export
setMethod("is_outcome", "purpose", function(object){ is_study_role(object, "outcome")})

#' @rdname is_study_role
#' @export
setGeneric("is_censoring", function(object) standardGeneric("is_censoring"))

#' @rdname is_study_role
#' @aliases is_censoring,purpose,purpose-method
#' @export
setMethod("is_censoring", "purpose", function(object){ is_study_role(object, "censoring")})

#' @rdname is_study_role
#' @export
setGeneric("is_competing", function(object) standardGeneric("is_competing"))

#' @rdname is_study_role
#' @aliases is_competing,purpose,purpose-method
#' @export
setMethod("is_competing", "purpose", function(object){ is_study_role(object, "competing")})

#' @rdname is_study_role
#' @export
setGeneric("is_exposure", function(object) standardGeneric("is_exposure"))

#' @rdname is_study_role
#' @aliases is_exposure,purpose,purpose-method
#' @export
setMethod("is_exposure", "purpose", function(object){ is_study_role(object, "exposure") })

#' @rdname is_study_role
#' @export
setGeneric("is_covariate", function(object) standardGeneric("is_covariate"))

#' @rdname is_study_role
#' @aliases is_covariate,purpose,purpose-method
#' @export
setMethod("is_covariate", "purpose", function(object){ is_study_role(object, "covariate") })

#' @rdname is_study_role
#' @export
setGeneric("is_weight", function(object) standardGeneric("is_weight"))

#' @rdname is_study_role
#' @aliases is_other,purpose,purpose-method
#' @export
setMethod("is_weight", "purpose", function(object){ is_study_role(object, "weight")})

#' @rdname is_study_role
#' @export
setGeneric("is_intermediate", function(object) standardGeneric("is_intermediate"))

#' @rdname is_study_role
#' @aliases is_other,purpose,purpose-method
#' @export
setMethod("is_intermediate", "purpose", function(object){ is_study_role(object, "intermediate")})

#' @rdname is_study_role
#' @export
setGeneric("is_other", function(object) standardGeneric("is_other"))

#' @rdname is_study_role
#' @aliases is_other,purpose,purpose-method
#' @export
setMethod("is_other", "purpose", function(object){ is_study_role(object, "other")})

#' Modify a purpose 
#' @name modify_purpose
#' @importFrom methods validObject
NULL

#' Add a study role
#' 
#' @rdname modify_purpose
#' @param object an object
#' @param role the roles to add
#' @export
setGeneric("add_study_role",
           function(object, role) standardGeneric("add_study_role"))

#' @rdname modify_purpose
#' @aliases add_study_role,purpose,purpose-method
#' @export
setMethod(
  "add_study_role", 
  c("purpose", "character"),
  function(object, role){ 
    out <- 
    set(d = object,
        l = slot_l("study_role"), 
        x = c(view(object, slot_l("study_role")), role))
    
    if ( validObject(out) ) out
  })

#' Add tags
#' 
#' @rdname modify_purpose
#' @param object an object
#' @param tags the tags to add/remove
#' @export
setGeneric("add_tags",
           function(object, tags) standardGeneric("add_tags"))

#' @rdname modify_purpose
#' @aliases add_tags,purpose,purpose-method
#' @export
setMethod(
  "add_tags", 
  c("purpose", "character"),
  function(object, tags){ 
    out <- 
      set(d = object,
          l = slot_l("tags"), 
          x = c(view(object, slot_l("tags")), tags))
    
    if ( validObject(out) ) out
  })

#' Remove a study role
#' 
#' @rdname modify_purpose
#' @param object an object
#' @param role the roles to remove
#' @export
setGeneric("remove_study_role", 
           function(object, role) standardGeneric("remove_study_role"))

#' @rdname modify_purpose
#' @aliases remove_study_role,purpose,purpose-method
#' @export
setMethod(
  "remove_study_role", 
  c("purpose", "character"),
  function(object, role){ 
    out <-
    set(d = object, 
        l = slot_l("study_role"), 
        x = setdiff(view(object, slot_l("study_role")), role))
    
    if ( validObject(out) ) out
  })

#' Remove tags
#' 
#' @rdname modify_purpose
#' @param object an object
#' @param tags the tags to add/remove
#' @export
setGeneric("remove_tags", 
           function(object, tags) standardGeneric("remove_tags"))

#' @rdname modify_purpose
#' @aliases remove_tags,purpose,purpose-method
#' @export
setMethod(
  "remove_tags", 
  c("purpose", "character"),
  function(object, tags){ 
    out <-
      set(d = object, 
          l = slot_l("tags"), 
          x = setdiff(view(object, slot_l("tags")), tags))
    
    if ( validObject(out) ) out
  })
