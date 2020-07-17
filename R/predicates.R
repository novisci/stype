#' Useful stype predicates
#' @name stype_predicates
#' @param x a \code{stype}
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
