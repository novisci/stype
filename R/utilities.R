#' String Representation of the Statistical Type
#'
#' Provides a short phrase identifying the statistical type.
#'
#' @param x A stype object.
#' @export
stype_str <- function(x) {
  assertthat::assert_that(
    is(x, "described"),
    msg = "not a stype object"
  )
  vec_ptype_full(x)
}


#' @importFrom crayon bold red combine_styles
boldmag <- crayon::combine_styles(crayon::bold, crayon::magenta)


context_printer <- function(x){
  ctxt <- get_context(x)
  if(is_empty(ctxt)){ return("")}
  prps <- methods::slot(get_purpose(ctxt), "study_role") 
  prps <- if(length(prps) == 0 || prps == "") "<undefined>" else prps
  sprintf("Purpose: %s\n", prps)
}

desc_printer <- function(x, label, what){
  sprintf("%s = %.3f", label, get_data_summary(x, what))
}

missing_printer <- function(x){
  if (get_data_summary(x, "has_missing")){
    boldmag(desc_printer(x, "Missing", "n_missing"))
  } else {
    ""
  }
}

footer_printer <- function(x, stats){
  
  assertthat::assert_that(
    length(names(stats)) == length(stats),
    msg = "stats must be a name vector."
  )
  
  sstats <- paste0(purrr::imap(
     .x = stats,
     .f = ~ desc_printer(x, .x, .y)),
     collapse = "; ")
  
  cxtp <- context_printer(x)
  miss <- missing_printer(x)
  
  cat(sstats %+% {if (miss != "") "; " else ""} %+% miss %+% "\n" %+%
      cxtp,
      sep = "")
}
