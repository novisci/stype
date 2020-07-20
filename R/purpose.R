#' Purpose object
#' 
#' @slot study_role the study role
#' @importFrom purrr walk
#' @importFrom methods slot slotNames new
#' @importFrom assertthat validate_that
#' @export purpose

purpose <- setClass(
  "purpose",
  slots = c(
    "study_role" = "character",
    "tags"       = "character"
  ),
  prototype = methods::prototype(
    study_role = "",
    tags       = ""
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
      methods::slot(object, "study_role") %in% valid_roles,
      msg = sprintf("study_role slot must be on of %s.", paste(valid_roles, collapse = ", "))
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
    purp <- methods::slot(object, "study_role") 
    if(length(purp) == 0) {
      FALSE
    } else {
      purp == what
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
