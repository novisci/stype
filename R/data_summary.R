#' A Class Subclassing From the `methods::namedList` Class
#'
#' This is the class of object returned by a call to [`describe`] with a stype
#' vector as the input. The class currently does not extend the
#' [`methods::namedList-class`] class.
#' 
#' @export

data_summary <- setClass(
  "data_summary",
  contains = "namedList"
)

#' Is a data_summary computed?
#' @keywords internal
is_not_computed <- function(x){
  assertthat::assert_that(
    is(x, "data_summary"),
    msg = "x should be a data_summary object.")
  
  length(x) == 0
}
