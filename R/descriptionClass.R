#' A call to describe returns a description object
#' 
#' @export description

description <- setClass(
  "description",
  contains = "namedList"
)

