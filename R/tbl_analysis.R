#' Analysis 
#' 
#' @description {
#' Prototype: \code{\link{tibble}}
#' }
#' 
#' @name tbl_analysis
#' @importFrom methods setOldClass
#' @inheritParams v_count
#' @family stype types
NULL

#' The internal builder of tbl_analysis
#' @noRd
#' @importFrom tibble tibble new_tibble
#' @keywords internal
new_tbl_analysis <- function(x = tibble::tibble(),                     
                       .internal_name = character(), 
                       .data_summary = data_summary(), 
                       .context = context(),
                       .extra_descriptors = list(),
                       .modifiers = list(function(dt) identity(dt))){
  
  stopifnot(is.data.frame(x))
  
  x <- purrr::reduce(
    .x = .modifiers,
    .f = ~ .y(.x),
    .init = x)

  tibble::new_tibble(
    x, 
    internal_name = .internal_name,
    data_summary  = .data_summary, 
    context       = .context, 
    extra_descriptors = .extra_descriptors,
    modifiers         = .modifiers,
    nrow  = nrow(x),
    class = c("tbl_analysis"))
}

#' @importFrom methods setOldClass
methods::setOldClass(c("tbl_analysis", "tbl_df", "tbl", "data.frame"))

#' Analysis constructor
#' @param x a \code{tibble}
#' @param modifiers a \code{list} of functions sequentially modify \code{x}.
#'     Defaults to simply the \code{identity} function
#' @importFrom tibble as_tibble
#' @rdname tbl_analysis 
#' @export
analysis <- function(x = tibble::tibble(),
                    internal_name = "", 
                    context,
                    extra_descriptors = list(),
                    modifiers = list(function(dt) identity(dt))){
  
  dsum <- describe(x, .descriptors = extra_descriptors)
  
  if(missing(context)){
    context <- methods::new("context")
  }
  
  new_tbl_analysis(
    x              = tibble::as_tibble(x),
    .internal_name = check_internal_name(internal_name),
    .data_summary  = dsum,
    .context       = context,
    .extra_descriptors = extra_descriptors,
    .modifiers         = modifiers)
  
}

#' Predicate function for analysis objects
#' @rdname tbl_analysis
#' @export
is_analysis <- function(x){
  inherits(x, "tbl_analysis")
}

#' @rdname tbl_analysis
#' @export
as_canonical.tbl_analysis <- function(x){
  as.data.frame(x)
}

#' @export
`[.tbl_analysis` <- function(x, i, j, drop = FALSE, ...){
  vec_restore(NextMethod("["), x)
}

#' @export
#' @method vec_restore tbl_analysis 
vec_restore.tbl_analysis <- function(x, to, ..., n = NULL) {

  ctxt  <- get_context(to)
  iname <- attr(to, "internal_name")
  edesc <- attr(to, "extra_descriptors")
  modfs <- attr(to, "modifiers")
  
  analysis(
    x,
    internal_name = iname, 
    context = ctxt,
    extra_descriptors = edesc,
    modifiers = modfs)
}
