#' Useful stype predicates
#' @name stype_predicates
#' @param x a \code{stype} or other object
#' @param tag a length 1 \code{character} vector
#' @param tags a \code{character} vector
NULL

#' @describeIn  stype_predicates is this vector constant?
#' @export
is_constant <- function(x){
  if (!is_stype(x)) return(FALSE)
  get_data_summary(x, "is_constant")
}

#' @describeIn  stype_predicates is this vector not constant?
#' @export
is_not_constant <- function(x) !is_constant(x)

#' @describeIn  stype_predicates Is this object tagged?
#' @export
setGeneric("is_tagged", function(x) standardGeneric("is_tagged"))

#' @describeIn  stype_predicates Is this \code{\linkS4class{purpose}} tagged?
#' @export
setMethod("is_tagged", c("purpose"), 
          function(x){ length(x@tags) > 0 && !all(x@tags == "")})

#' @describeIn  stype_predicates Is this \code{\link{context}} tagged?
#' @export
setMethod("is_tagged", c("context"), function(x){ is_tagged(get_purpose(x)) })

#' @describeIn  stype_predicates Is this \code{\link{stype}} tagged?
#' @export
setMethod("is_tagged", c("stype"), function(x){ is_tagged(get_context(x)) })

#' @describeIn  stype_predicates Is this non-stype tagged?  \code{FALSE}
#' @export
setMethod("is_tagged", c("ANY"), function(x){ FALSE })

#' @describeIn  stype_predicates Is this object not tagged?
#' @export
setGeneric("is_not_tagged", function(x) standardGeneric("is_not_tagged"))

#' @describeIn  stype_predicates Is this non-stype tagged?  \code{FALSE}
#' @export
setMethod("is_not_tagged", c("ANY"), function(x){ !is_tagged(x) })

#' @describeIn  stype_predicates Does \code{x} have the \code{tag}?
#' @export
setGeneric("has_tag", function(x, tag) standardGeneric("has_tag"))

#' @describeIn  stype_predicates Does \code{\linkS4class{purpose}} have \code{tag}?
#' @export
setMethod(
  "has_tag", 
  c("purpose"), 
  function(x, tag){ 
    
    assertthat::assert_that(
      length(tag) == 1L,
      msg = "tag must be length 1. You may want has_any_tags, has_all_tags, or has_only_tags."
    )
    
    tag %in% x@tags 
  })

#' @describeIn  stype_predicates Does \code{\link{context}} have \code{tag}?
#' @export
setMethod("has_tag", c("context"), function(x, tag){ has_tag(get_purpose(x), tag) })

#' @describeIn  stype_predicates Does \code{\link{stype}} have \code{tag}?
#' @export
setMethod("has_tag", c("stype"), function(x, tag){ has_tag(get_context(x), tag) })

#' @describeIn  stype_predicates Does this non-stype have \code{tag}?
#' @export
setMethod("has_tag", c("ANY"), function(x, tag){ is_tagged(x) })

#' @describeIn stype_predicates Does \code{x} have any of the \code{tags}?
#' @export
setGeneric("has_any_tags", function(x, tags) standardGeneric("has_any_tags"))

#' @describeIn  stype_predicates Does \code{x} have any of the \code{tags}?
#' @export
setMethod("has_any_tags", c("ANY"), 
          function(x, tags){ any(purrr::map_lgl(tags, ~ has_tag(x, .x))) })

#' @describeIn stype_predicates Does \code{x} have all of the \code{tags}?
#' @export
setGeneric("has_all_tags", function(x, tags) standardGeneric("has_all_tags"))

#' @describeIn  stype_predicates Does \code{x} have all of the \code{tags}?
#' @export
setMethod("has_all_tags", c("ANY"), 
          function(x, tags){ all(purrr::map_lgl(tags, ~ has_tag(x, .x))) })

#' @describeIn stype_predicates Does \code{x} have only the \code{tags}?
#' @export
setGeneric("has_only_tags", function(x, tags) standardGeneric("has_only_tags"))

#' @describeIn  stype_predicates Does \code{x} have only the \code{tags}?
#' @export
setMethod("has_only_tags", c("purpose"),  
          function(x, tags){ identical(union(x@tags, tags),
                                       intersect(x@tags, tags)) })

#' @describeIn  stype_predicates Does \code{x} have only the \code{tags}?
#' @export
setMethod("has_only_tags", c("context"),
          function(x, tags){ has_only_tags(get_purpose(x), tags) })

#' @describeIn  stype_predicates Does \code{x} have only the \code{tags}?
#' @export
setMethod("has_only_tags", c("stype"),
          function(x, tags){ has_only_tags(get_context(x), tags) })

#' @describeIn  stype_predicates Does \code{x} have only the \code{tags}?
#' @export
setMethod("has_only_tags", c("ANY"), function(x, tags){ is_tagged(x) })