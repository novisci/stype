#' Ordered categorical vectors
#' 
#' Constructors and methods for an ordered categorical data type.
#' \code{v_ordered} and \code{ord} are synonyms that each create a new
#' \code{v_ordered} object subclassed from \code{vctrs_vctr}, \code{ordered},
#' and \code{factor}. \cr\cr
#' Support: see \code{\link{factor}} \cr
#' Prototype: \code{\link{integer}}
#'
#' @name v_ordered
#' @inheritParams v_binary
#' @importFrom vctrs vec_ptype2.character
#' @family stype types
#' @examples
#' # Example data
#' src_fct <- factor(c("a", "bb", "a", "ccc", NA_character_), ordered = TRUE)
#'
#' # Constructor for the `v_ordered` class. One can also use `ord` which is a
#' # synonym for the `v_ordered` function.
#' v <- v_ordered(
#'   x = src_fct,
#'   internal_name = "v_example",
#'   context = context(
#'     short_label = "important_var",
#'     long_label  = "Very important variable"
#'   ),
#'   extra_descriptors = list()
#' )
#'
#' # Helper functions and methods
#' is_ordered(v)
#' as_ordered(src_fct)
#' as.character(v)
#' as_canonical(v)
NULL

#' The internal builder of v_ordered
#' @noRd
#' @param .levels Character vector of labels.
#' @keywords internal
new_ordered <- function(x = integer(), .levels = character(),
                        .internal_name = character(), 
                        .data_summary = data_summary(), 
                        .context = context(),
                        .auto_compute_summary = auto_compute_default,
                        .extra_descriptors = list()) {
  stopifnot(is.integer(x))
  stopifnot(is.character(.levels))
  
  new_stype_vctr(
    x,
    .internal_name = .internal_name,
    .data_summary  = .data_summary, 
    .context       = .context,
    .extra_descriptors = .extra_descriptors,
    .auto_compute_summary = .auto_compute_summary,
    .class   = c("v_ordered", "vctrs_vctr", "ordered"),
    # As of v0.3.8  vctrs::new_ordered doesn't take ... arguments, thus using
    # new_factor instead
    new_fun = vctrs::new_factor, 
    levels = .levels)
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_ordered", "vctrs_vctr"))

#' Construction for a v_ordered
#' @rdname v_ordered
#' @param x a \code{factor}
#' @importFrom assertthat assert_that
#' @export
v_ordered <- function(x = factor(ordered = TRUE),
                      internal_name = "", 
                      context,
                      auto_compute_summary = auto_compute_default,
                      extra_descriptors = list()){

  assert_that(is.ordered(x))
  
  # x <- vctrs::vec_cast(x, factor())
  
  assertthat::assert_that(
    is_truth(auto_compute_summary),
    msg = "auto_compute_summary must be TRUE or FALSE."
  )

  dsum <- 
    `if`(
      auto_compute_summary,
      describe(x, .descriptors = extra_descriptors ),
      data_summary() )
  
  
  if(missing(context)){
    context <- methods::new("context")
  }
  
  new_ordered(
    x        = as.integer(x),
    .levels  = levels(x), 
    .internal_name = check_internal_name(internal_name),
    .data_summary  = dsum,
    .context       = context,
    .auto_compute_summary = auto_compute_summary,
    .extra_descriptors = extra_descriptors)
}

#' @rdname v_ordered 
#' @export
ord <- v_ordered

#' Predicate function for ordered objects
#' @rdname v_ordered 
#' @export

is_ordered <- function(x){
  inherits(x, "v_ordered")
}

# Coerce ------------------------------------------------------------------
#' @rdname casting
#' @export vec_ptype2.v_ordered
#' @method vec_ptype2 v_ordered
#' @export
vec_ptype2.v_ordered <- function(x, y, ...) UseMethod("vec_ptype2.v_ordered", y)

#' @method vec_ptype2.character v_ordered
#' @export
vec_ptype2.character.v_ordered <- function(x, y, ...) character()
#' @method vec_ptype2.v_ordered character
#' @export
vec_ptype2.v_ordered.character <- function(x, y, ...) character()
#' @method vec_ptype2.v_ordered v_ordered
#' @export
vec_ptype2.v_ordered.v_ordered <- function(x, y, ...) {
  compare_contexts(x, y)
  check_internal_names(x, y)
  
  new_ordered(
    .levels =  union(levels(x),levels(y)),
    .internal_name = get_internal_name(x),
    .context = get_context(x),
    .auto_compute_summary = decide_auto_compute(x, y))
}

# Cast --------------------------------------------------------------------
#' @rdname casting
#' @export vec_cast.v_ordered
#' @method vec_cast v_ordered
#' @importFrom vctrs vec_cast.character
#' @export
vec_cast.v_ordered<- function(x, to, ...) {
  UseMethod("vec_cast.v_ordered")
}

#' @export
#' @method vec_cast.v_ordered v_ordered
vec_cast.v_ordered.v_ordered <- function(x, to, ..., x_arg = "", to_arg = "") {
  # TODO: I really hope there's a cleaner way
  # browser()
  if (length(levels(to)) == 0L) {
    x <- levels(x)[x]
    out        <- factor(x, levels = unique(x), ordered = TRUE)
    class(out) <- c("v_ordered", "vctrs_vctr", "ordered",  "factor")
  } else {
    lossy <- !(x %in% levels(to) | is.na(x))
    x <- levels(x)[x]
    out       <- factor(x, levels = levels(to), ordered = TRUE)
    class(out) <- c("v_ordered", "vctrs_vctr", "ordered", "factor")
    vctrs::maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
  }
}
#' @export
#' @method vec_cast.v_ordered character
vec_cast.v_ordered.character <- vec_cast.v_ordered.v_ordered

#' @export
#' @method vec_cast.character v_ordered
vec_cast.character.v_ordered <- function(x, to, ...) {
  levels(x)[vctrs::vec_data(x)]
}
#' @export
#' @method vec_cast.v_ordered list
vec_cast.v_ordered.list <- function(x, to, ..., x_arg = "", to_arg = "") {
  vctrs::vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
#' @method vec_restore v_ordered 
vec_restore.v_ordered <- function(x, to, ...,n = NULL) {
  
  # TODO: could we use make_stype_restorator
  ctxt  <- get_context(to)
  iname <- attr(to, "internal_name")
  edesc <- attr(to, "extra_descriptors")
  auto  <- attr(to, "auto_compute_summary")
  
  x   <- levels(to)[x]
  out <- factor(x, levels = levels(to), ordered = TRUE)
  
  v_ordered(
    out,
    internal_name = iname, 
    context = ctxt,
    auto_compute_summary = auto,
    extra_descriptors = edesc)
}

#' Casting function for ordered objects
#' @rdname v_ordered
#' @export
as_ordered <- function(x) {
  v_ordered(x)
}

#' @rdname v_ordered
#' @export
as_canonical.v_ordered <- function(x){
  factor(x, levels(x), ordered = TRUE)
}

#' @export
#' @method levels v_ordered 

levels.v_ordered <- levels.default

# Math and arithmetic -----------------------------------------------------
# @export
# vec_math.v_ordered <- function(.fn, .x, ...) {
#   stop_unsupported(.x, .fn)
# }

# @export
# vec_arith.v_ordered <- function(op, x, y, ...) {
#   vctrs::stop_unsupported(x, op)
# }

# Formatting ####
#' @method format v_ordered
#' @export
format.v_ordered <- function(x, ...) {
  out <- levels(x)[x]
  out[is.na(x)] <- NA
  out
}

# Print foot
#' @importFrom vctrs obj_print_footer
#' @method obj_print_footer v_ordered
#' @export
obj_print_footer.v_ordered <- function(x, ...) {
  
  print_footer(
    x, 
    stats = list(
      ptable = list(
        label = "Proportions",
        printer  = function(x, label) {
          r <- paste0(paste0(dimnames(x)[[1]], ": ", round(x, 2)*100, "%"), 
                      collapse = " ")
          sprintf("%s: %s", label, r)
        }
      )
    ))
}


#' @export
vec_ptype_full.v_ordered <- function(x, ...) {
  "ordered"
  # paste0("ordered<", hash_label(levels(x)), ">")
}

#' @export
vec_ptype_abbr.v_ordered <- function(x, ...) {
  "ord"
}

#' @importFrom pillar type_sum
#' @export
type_sum.v_ordered <- function(x) {
  "ord"
}

# Helpers -----------------------------------------------------------------

# hash_label <- function(x, length = 5) {
#   if (length(x) == 0) {
#     ""
#   } else {
#     # Can't use hash() currently because it hashes the string pointers
#     # for performance, so the values in the test change each time
#     substr(digest::digest(x), 1, length)
#   }
# }
