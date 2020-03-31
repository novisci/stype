#' String Representation of the Statistical Type
#'
#' Provides a short phrase identifying the statistical type.
#'
#' @param x A stype object.
#' @return A string.
#' @export
stype_str <- function(x) {
  assertthat::assert_that(
    is(x, "stype"),
    msg = "not a stype object"
  )
  vctrs::vec_ptype_full(x)
}


#' @importFrom crayon bold red combine_styles
boldmag <- crayon::combine_styles(crayon::bold, crayon::magenta)

#' A printer for stype contexts
#' @noRd
#' @param x a \code{stype} variable
print_context <- function(x){
  ctxt <- get_context(x)
  if(is_empty(ctxt)){ return("")}
  prps <- methods::slot(get_purpose(ctxt), "study_role") 
  prps <- if(length(prps) == 0 || prps == "") "<undefined>" else prps
  sprintf("Purpose: %s\n", prps)
}

#' A printer for stype data_summaries
#' @noRd
#' @param x a \code{stype} variable
#' @param label the label to use on the LHS of "="
#' @param what what to show on the RHS of "="
print_data_summary <- function(x, label, what){
  sprintf("%s = %.3f", label, get_data_summary(x, what))
}

#' A printer for the has_missing item
#' @noRd
#' @param x a \code{stype} variable
print_missing <- function(x){
  if (get_data_summary(x, "has_missing")){
    boldmag(print_data_summary(x, "Missing", "n_missing"))
  } else {
    ""
  }
}

#' A printer for the footer of a stype
#' @noRd
#' @param x a \code{stype} variable
#' @param stats a named \code{character} vector whose names identify which items from the 
#' \code{data_summary} to include in the footer and the elements are the labels
#' @importFrom purrr imap 
#' @importFrom crayon "%+%"
print_footer <- function(x, stats){
  
  assertthat::assert_that(
    length(names(stats)) == length(stats),
    msg = "stats must be a named vector."
  )
  
  sstats <- paste0(purrr::imap(
     .x = stats,
     .f = ~ print_data_summary(x, .x, .y)),
     collapse = "; ")
  
  cxtp <- print_context(x)
  miss <- print_missing(x)
  
  cat(sstats %+% {if (miss != "") "; " else ""} %+% miss %+% "\n" %+%
      cxtp,
      sep = "")
}

#' Check that internal names are equal
#' @param x a stype vector
#' @param y another stype vector
#' @keywords internal

check_internal_names <- function(x, y){
  assertthat::assert_that(
    get_internal_name(x) == get_internal_name(y),
    msg = "internal names of x and y must match"
  )
}
