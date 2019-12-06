#' String Representation of the Statistical Type
#'
#' Provides a short phrase identifying the statistical type.
#'
#' @param x An R object.
#' @export
stype_str <- function(x) {
  UseMethod("stype_str")
}


#' @export
stype_str.v_binary <- function(x) {
  "binary"
}


#' @export
stype_str.v_character <- function(x) {
  "character"
}


#' @export
stype_str.v_continuous <- function(x) {
  "continuous"
}


#' @export
stype_str.v_continuous_nonneg <- function(x) {
  "continuous nonnegative"
}


#' @export
stype_str.v_count <- function(x) {
  "count"
}


#' @export
stype_str.v_event_time <- function(x) {
  "time-to-event"
}


#' @export
stype_str.v_nominal <- function(x) {
  "nominal"
}


#' @export
stype_str.v_ordered <- function(x) {
  "ordered"
}
