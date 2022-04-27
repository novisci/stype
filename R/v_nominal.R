#' Nominal categorical vectors
#'
#' Constructors and methods for a nomimal categorical data type.
#' \code{v_nominal} and \code{nom} are synonyms that each create a new
#' \code{v_nominal} object subclassed from \code{vctrs_vctr} and \code{factor}.
#' \cr\cr
#' Support: see \code{\link{factor}} \cr
#' Prototype: \code{\link{integer}}
#'
#' @name v_nominal
#' @inheritParams v_binary
#' @importFrom vctrs vec_ptype2.character
#' @family stype types
#' @examples
#' # Example data
#' src_fct <- factor(c("a", "bb", "a", "ccc", NA_character_))
#'
#' # Constructor for the `v_nominal` class. One can also use `nom` which is a
#' # synonym for the `v_nominal` function.
#' v <- v_nominal(
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
#' is_nominal(v)
#' as_nominal(src_fct)
#' as.character(v)
#' as_canonical(v)
NULL

#' The internal builder of v_nominal
#' @noRd
#' @param .levels Character vector of labels.
#' @keywords internal
new_nominal <- function(x = integer(), 
                        .levels = character(),
                        .data_summary = data_summary(), 
                        .internal_name = character(), 
                        .context = context(),
                        .auto_compute_summary = auto_compute_default,
                        .extra_descriptors = list()){
  
  stopifnot(is.integer(x))
  stopifnot(is.character(.levels))
  
  new_stype_vctr(
    x,
    .internal_name = .internal_name,
    .context       = .context,
    .data_summary  = .data_summary,
    .extra_descriptors = .extra_descriptors,
    .auto_compute_summary = .auto_compute_summary,
    .class   = c("v_nominal", "vctrs_vctr"),
    new_fun = vctrs::new_factor,
    levels = .levels)

}
#' @importFrom methods setOldClass
methods::setOldClass(c("v_nominal", "vctrs_vctr"))

#' Construction for a v_nominal
#' @rdname v_nominal
#' @param x a \code{factor}
#' @importFrom assertthat assert_that
#' @export
v_nominal <- function(x = factor(), 
                      internal_name = "", 
                      context,
                      auto_compute_summary = auto_compute_default,
                      extra_descriptors = list()){

  assert_that(is.factor(x))

  # x <- vctrs::vec_cast(x, factor())
  
  assertthat::assert_that(
    is_truth(auto_compute_summary),
    msg = "auto_compute_summary must be TRUE or FALSE."
  )
  
  dsum <- 
    `if`(
      auto_compute_summary,
      describe( x, .descriptors = extra_descriptors ),
      data_summary() )
  
  if(missing(context)){
    context <- methods::new("context")
  }
  
  new_nominal(
    x        = as.integer(x),
    .levels  = levels(x), 
    .internal_name = check_internal_name(internal_name),
    .data_summary  = dsum,
    .context       = context,
    .auto_compute_summary = auto_compute_summary,
    .extra_descriptors = extra_descriptors)
}

#' @rdname v_nominal
#' @export
nom <- v_nominal

#' Predicate function for nominal objects
#' @rdname v_nominal 
#' @export

is_nominal <- function(x){
  inherits(x, "v_nominal")
}

# Coerce ------------------------------------------------------------------
#' @rdname casting
#' @export vec_ptype2.v_nominal
#' @method vec_ptype2 v_nominal
#' @export
vec_ptype2.v_nominal <- function(x, y, ...) UseMethod("vec_ptype2.v_nominal", y)

#' @method vec_ptype2.character v_nominal
#' @export
vec_ptype2.character.v_nominal <- function(x, y, ...) character()
#' @method vec_ptype2.v_nominal character
#' @export
vec_ptype2.v_nominal.character <- function(x, y, ...) character()
#' @method vec_ptype2.v_nominal v_nominal
#' @export
vec_ptype2.v_nominal.v_nominal <- function(x, y, ...) {
  compare_contexts(x, y)
  check_internal_names(x, y)
  
  new_nominal(
    .levels =  union(levels(x),levels(y)),
    .internal_name = get_internal_name(x),
    .context = get_context(x),
    .auto_compute_summary = decide_auto_compute(x, y))
}

# Cast --------------------------------------------------------------------
#' @rdname casting
#' @export vec_cast.v_nominal
#' @method vec_cast v_nominal
#' @importFrom vctrs vec_cast.character
#' @export
vec_cast.v_nominal<- function(x, to, ...) {
  UseMethod("vec_cast.v_nominal")
}

#' @export
#' @method vec_cast.v_nominal v_nominal
vec_cast.v_nominal.v_nominal <- function(x, to, ..., x_arg = "", to_arg = "") {
  # TODO: I really hope there's a cleaner way
  # browser()
  if (length(levels(to)) == 0L) {
    x <- levels(x)[x]
    out        <- factor(x, levels = unique(x))
    class(out) <- c("v_nominal", "vctrs_vctr", "factor")
  } else {
    lossy <- !(x %in% levels(to) | is.na(x))
    x <- levels(x)[x]
    out       <- factor(x, levels = levels(to))
    class(out) <- c("v_nominal", "vctrs_vctr", "factor")
    vctrs::maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
  }
}
#' @export
#' @method vec_cast.v_nominal character
vec_cast.v_nominal.character <- vec_cast.v_nominal.v_nominal

#' @export
#' @method vec_cast.character v_nominal
vec_cast.character.v_nominal <- function(x, to, ...) {
  levels(x)[vctrs::vec_data(x)]
}
#' @export
#' @method vec_cast.v_nominal list
vec_cast.v_nominal.list <- function(x, to, ..., x_arg = "", to_arg = "") {
  vctrs::vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
#' @method vec_restore v_nominal 
vec_restore.v_nominal <- function(x, to, ..., n = NULL) {
  
  # TODO: could we use make_stype_restorator
  ctxt  <- get_context(to)
  iname <- attr(to, "internal_name")
  edesc <- attr(to, "extra_descriptors")
  auto  <- attr(to, "auto_compute_summary")
  
  x   <- levels(to)[x]
  out <- factor(x, levels = levels(to))
  
  v_nominal(
    out,
    internal_name = iname, 
    context = ctxt,
    auto_compute_summary = auto,
    extra_descriptors = edesc)
}

#' Casting function for nominal objects
#' @rdname v_nominal
#' @export
as_nominal <- function(x) {
  v_nominal(x)
}

#' @rdname v_nominal
#' @export
as_canonical.v_nominal <- function(x){
  factor(x, levels(x))
}

#' @export
#' @method levels v_nominal 

levels.v_nominal <- levels.default

# Math and arithmetic -----------------------------------------------------
# @export
# vec_math.v_nominal <- function(.fn, .x, ...) {
#   stop_unsupported(.x, .fn)
# }

# @export
# vec_arith.v_nominal <- function(op, x, y, ...) {
#   vctrs::stop_unsupported(x, op)
# }

# Print -------------------------------------------------------------------

# Formatting ####
#' @method format v_nominal
#' @export
format.v_nominal <- function(x, ...) {
  out <- levels(x)[x]
  out[is.na(x)] <- NA
  out
}

# Print foot
#' @importFrom vctrs obj_print_footer
#' @method obj_print_footer v_nominal
#' @export
obj_print_footer.v_nominal <- function(x, ...) {
  
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
vec_ptype_full.v_nominal <- function(x, ...) {
  "nominal"
  # paste0("nominal<", hash_label(levels(x)), ">")
}

#' @export
vec_ptype_abbr.v_nominal <- function(x, ...) {
  "nom"
}

#' @importFrom pillar type_sum
#' @export
type_sum.v_nominal <- function(x) {
  "nom"
}
