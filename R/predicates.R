#' Useful stype predicates
#' @name stype_predicates
#' @param x a \code{stype} or other object
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
setGeneric("is_tagged", function(x, tags) standardGeneric("is_tagged"))

#' @describeIn  stype_predicates Is this \code{\linkS4class{purpose}} tagged with
#'      \code{tags}?
#' @export
setMethod("is_tagged", c("purpose", "character"),
          function(x, tags){ any(tags %in% x@tags) })

#' @describeIn  stype_predicates Is this \code{\link{context}} tagged with
#'      \code{tags}?
#' @export
setMethod("is_tagged", c("context", "character"),
          function(x, tags){ is_tagged(get_purpose(x), tags) })

#' @describeIn  stype_predicates Is this \code{\link{stype}} tagged with
#'      \code{tags}?
#' @export
setMethod("is_tagged", c("stype", "character"),
          function(x, tags){ is_tagged(get_context(x), tags) })

#' @describeIn  stype_predicates Is this non-stype tagged with \code{tags}?
#'      \code{FALSE}
#' @export
setMethod("is_tagged", c("ANY", "character"), function(x, tags){ FALSE })