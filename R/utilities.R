#' The internal builder of (basic) stype vctrs
#' 
#' Used instead of `vctrs::new_vctr` in each builder in order to keep the 
#' attributes consistent across stype vctrs.
#' 
#' @param x the vector data
#' @param .internal_name `internal_name` attribute 
#' @param .data_summary [`data_summary`] attribute
#' @param .context [`context`] attribute
#' @param .extra_descriptor `extra_descriptors` attribute a `list` of descriptor
#'  functions
#' @param .class passed to the `class` argument of (e.g.) the `new_fun`
#' @param new_fun the `vctrs` `new_*` function to use. Defaults to 
#'    [`vctrs::new_vctr()`]. The first argument must the "data" argument called 
#'    ".data" in new_vctr and "x" in new_factor and "fields" in new_rcrd.
#' @keywords internal
#' @importFrom vctrs new_vctr
new_stype_vctr <- function(x, .internal_name, .data_summary, .context,
                           .extra_descriptors, .auto_compute_summary,
                           .class,
                           new_fun = vctrs::new_vctr,
                           ...){
  
  check_internal_name(.internal_name)
  
  new_fun(
    x, 
    ... ,
    # Attributes available to be set by developer
    internal_name = .internal_name,
    data_summary  = .data_summary, 
    context       = .context, 
    extra_descriptors = .extra_descriptors,
    auto_compute_summary = .auto_compute_summary,
    
    # Automatic attributes
    # stype version is kept in order to do compatibility checks in the future
    stype_version = utils::packageVersion(pkg = "stype"),
    
    # Class
    class = .class)
}

#' Create a constructor function for stype vectors
#' 
#' @param typeFUN the \code{new_} function that creates vdctors of the type
#' @param ptypeFUN the prototype of the vector
#' @param castFUN optionally cast the data by \code{\link[vctrs]{vec_cast}}
#' @param dataFUN a function to apply to the data before \code{describe}ing the
#'     the data. Defaults to \code{identity}.
#' @keywords internal
make_stype_constructor <- function(typeFUN, 
                                   ptypeFUN, 
                                   castFUN = ptypeFUN, 
                                   dataFUN = identity){
  function(x = ptypeFUN(), 
           internal_name = "", 
           context, 
           auto_compute_summary = auto_compute_default,
           extra_descriptors = list()){

      x <- vctrs::vec_cast(x, castFUN())
      vctrs::vec_assert(x, ptype = ptypeFUN())
      
      assertthat::assert_that(
        is_truth(auto_compute_summary),
        msg = "auto_compute_summary must be TRUE or FALSE."
      )
      
      dsum <- 
        `if`(
          auto_compute_summary,
            describe( dataFUN(x), .descriptors = extra_descriptors ),
            data_summary() )
      
      if(missing(context)){
        context <- methods::new("context")
      }

      typeFUN(
        x,
        .internal_name = check_internal_name(internal_name),
        .data_summary = dsum,
        .context = context,
        .auto_compute_summary = auto_compute_summary,
        .extra_descriptors = extra_descriptors
      )
  }
}

#' Create a vec_restore function for basic stype types
#' @inheritParams make_stype_constructor
#' @keywords internal
make_stype_restorator <- function(typeFUN){
  function(x, to, ..., n = NULL) {
    
    # Maintain meta-info
    iname   <- attr(to, "internal_name")
    edesc   <- attr(to, "extra_descriptors")
    context <- get_context(to)
    # Update data summary
    auto    <- attr(to, "auto_compute_summary")
    dsum <- 
      `if`(
        auto,
        describe( vctrs::vec_data(x), .descriptors = edesc ),
        data_summary() )


    typeFUN(
      x, 
      .internal_name = iname, 
      .data_summary = dsum, 
      .context = context,
      .extra_descriptors = edesc,
      .auto_compute_summary = auto)
  }
}

#' Create a stype<->stype ptype2 function
#' @param constructorFUN the constructor function to use.
#' @keywords internal
make_stype_ptype2 <- function(constructorFUN){
  function(x, y, ...){
    compare_contexts(x, y)
    check_internal_names(x, y)
    
    constructorFUN(internal_name = get_internal_name(x), 
                   context = get_context(x),
                   auto_compute_summary = decide_auto_compute(x, y))
  }

}
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
  prps <- if(length(prps) == 0) "<undefined>" else prps
  sprintf("Purpose: %s\n", paste(prps, collapse = ", "))
}

#' A printer for basic stype data_summaries
#' @noRd
#' @param x a numeric value
#' @param label the label to use on the LHS of "="
#' @param format the format to use for the value
print_numeric_summary <- function(x, label, format = '%.3f'){
  fmt <- paste0("%s = ", format)
  sprintf(fmt, label, x)
}

#' A printer for the has_missing item
#' @noRd
#' @param x a \code{stype} variable
print_missing <- function(x){
  if (get_data_summary(x, "has_missing")){
    boldmag(print_numeric_summary(get_data_summary(x, "n_missing"), "Missing", '%d'))
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
  
  # if the data summary is empty, there's no footer to print.
  if (is_not_computed(attr(x, "data_summary"))){
    return(x)
  }
  
  # If the length of the vector is 0, there's no footer to print
  if (length(x) == 0) { return(x) }
  
  assertthat::assert_that(
    length(names(stats)) == length(stats),
    msg = "stats must be a named vector."
  )
  
  sstats <- paste0(purrr::imap(
     .x = stats,
     .f = ~ {
       .x$printer(get_data_summary(x, .y), .x$label)
     }),
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

#' In the case that both x and y not equal to the default,
#' return not the default;
#' otherwise, return the default
#' @keywords internal
decide_auto_compute <- function(x, y){
  x <- attr(x, "auto_compute_summary")
  y <- attr(y, "auto_compute_summary")
  if (x != auto_compute_default && y != auto_compute_default ) !auto_compute_default
  else  auto_compute_default
}

#' Is this data_summary to be auto computed?
#' @keywords internal
is_auto_computed <- function(x){
  attr(x, "auto_compute_summary")
}

#' Swap the called function for a different function
#' @param cl a call
#' @param fn the new function
#' @keywords internal
swap_function <- function(cl, fn){
  cl[[1]] <- fn
  cl
}

#' Check that a character vector has length 0 or 1
#' @param x what to check
#' @keywords internal
check_internal_name <- function(x){
  assertthat::assert_that(
    is.character(x) && length(x) <= 1,
    msg = "An internal name should be a single string"
  )
  x
}

#' Check whether a value is a positive scalar number
#' @param x what to check
#' @keywords internal
check_number_positive <- function(x){
  is.numeric(x) && (length(x) == 1L) && (! is.na(x)) && (x > 0)
}

#' Check if an object is either TRUE or FALSE
#' @param x what to check
#' @noRd
is_truth <- function(x) {
  (is.logical(x)
    && (length(x) == 1L)
    && (! is.na(x))
  )
}

#' Create custom \code{rlang::abort} Class for Stype Objects
#'
#' modeled after \code{vctrs:::stop_vctrs}
#' @param msg a string containing the error message.
#' @param class a character vector with error classes, if any, to add in
#' addition to "stype_err".
#' @param ... additional arguments passed to \code{abort}
#' @importFrom rlang abort
#' @keywords internal
stype_abort  <- function(msg = NULL, class = NULL, ...) {
  abort(msg, class = c(class, "stype_err"), ...)
}

#' Throw 'stype_invalid_math' Error
#'
#' Helper function used to throw an error with a standardized class for the case
#' where an unsupported math operation is called for a given stype type. This is
#' intended to be explicitly invoked by internal routines in situations where we
#' don't want the S3 dispatch to fall through to vctrs handling of math
#' operators, which amounts to invoking the operator in base with the raw data
#' as the input for types that are supported, and then restoring the appropriate
#' vctrs but not stype type (for types that are not supported by vctrs an error
#' is thrown immediately).
#' @param x stype vector
#' @param op the math operation
#' @param class optional class argument passed to \code{stype_abort}
#' @param ... additional arguments passed to \code{stype_abort}
#' @keywords internal
stop_invalid_math <- function(x, op, class = NULL, ...) {
  msg  <- c(sprintf("%s is not defined for stype class %s.", op, class(x)[1L]),
            i = "Try casting to the stype prototype with as_canonical.")
  stype_abort(msg, class = c(class, "stype_invalid_math"), ...)
}

#' Creates a function for getting a summary value
#' 
#' @param summary_name name of the value in the stype's \code{data_summary}
#' @param fn the function to use in the case that the summary is not already computed.
#' @keywords  internal
#' @seealso maybe_get_data_summary_math
make_maybe_get_summary <- function(summary_name, fn) {
  function(x, na.rm, ...){
    `if`(is_auto_computed(x) && na.rm,
         get_data_summary(x, summary_name),
         fn(vctrs::vec_data(x), na.rm = na.rm, ...))
  }
}

#' Gets a summary value already computed in a data_summary
#' 
#' Gets a value from a stype vector's data_summary in the case the summary is 
#' auto computed AND the `na.rm` argument is TRUE.
#' @param summary_name name of the value in the stype's \code{data_summary}
#' @inheritParams vctrs::vec_math_base
#' @keywords  internal
maybe_get_data_summary_math <- function(summary_name, .fn, .x, ...){
  dots <- list(...)
  `if`(is_auto_computed(.x) && dots$na.rm,
       get_data_summary(.x, summary_name),
       vec_math_base(.fn, .x, ...))
}

#' Checks that summary function ellipsis arguments only 
#' @keywords internal
check_summary_args <- function(...){
  dots <- list(...)

  assert_that(
    length(dots) == 1,
    msg = "stype does not support summary methods on more than 1 vector. Solution: Cast to the canonical type to relieve this itch."
  )
}

#' Get the codomain of math/summary function for stype vectors
#' @param fns character vector of vector names
#' @param x a dummy stype vector
#' @keywords internal
determine_mathish_codomain <- function(x, fns){
  
  purrr::map(
    fns,
    .f = ~ {
      attempt <- try(do.call(.x, args = list(x)), silent = TRUE)
      
      if (is(attempt, "try-error")) {
        "undefined"
      } else {
        vctrs::vec_ptype_full(attempt)
      }
      
    }
  )
}

#' Get Math codomains
#' @inheritParams determine_mathish_codomain
#' @importFrom methods Math Summary getGroupMembers
#' @keywords internal
get_math_codomains <- function(x){
  mt <- getGroupMembers(Math)
  determine_mathish_codomain(x, setNames(mt, mt))
}

#' Get Summary codomains
#' @inheritParams determine_mathish_codomain
#' @keywords internal
get_summary_codomains <- function(x){
  mt <- getGroupMembers(Summary)
  determine_mathish_codomain(x, setNames(mt, mt))
}
