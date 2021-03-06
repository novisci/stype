#' Nominal categorical vectors
#' 
#' @description  {
#' Support: see \code{\link{factor}}
#' 
#' Prototype: \code{\link{integer}}
#' }
#' @name v_nominal
#' @inheritParams v_count
#' @importFrom vctrs vec_ptype2.character
#' @family stype types
NULL

#' The internal builder of v_nominal
#' @noRd
#' @param .levels Character vector of labels.
#' @keywords internal
new_nominal <- function(x = integer(), 
                        .levels = character(),
                        .internal_name = character(), 
                        .data_summary = data_summary(), 
                        .context = context(),
                        .extra_descriptors = list()){
  
  stopifnot(is.integer(x))
  stopifnot(is.character(.levels))
  
  structure(
    x,
    levels  = .levels,
    internal_name = .internal_name,
    data_summary  = .data_summary, 
    context       = .context,
    extra_descriptors = .extra_descriptors,
    class   = c("v_nominal", "vctrs_vctr", "factor")
  )
}
#' @importFrom methods setOldClass
methods::setOldClass(c("v_nominal", "vctrs_vctr"))

#' Construction for a v_nominal
#' @rdname v_nominal
#' @param x a \code{factor}
#' @export
v_nominal <- function(x = factor(), internal_name = "", context,
                      extra_descriptors = list()){
  
  # x <- vctrs::vec_cast(x, factor())
  dsum <- describe(x, .descriptors = extra_descriptors)
  
  if(missing(context)){
    context <- methods::new("context")
  }
  
  new_nominal(
    x        = as.integer(x),
    .levels  = levels(x), 
    .internal_name = check_internal_name(internal_name),
    .data_summary  = dsum,
    .context       = context,
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
    .context = get_context(x))
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
  
  x   <- levels(to)[x]
  out <- factor(x, levels = levels(to))
  
  v_nominal(
    out,
    internal_name = iname, 
    context = ctxt,
    extra_descriptors = edesc)
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
  # TODO: use print_footer
  ptab <- get_data_summary(x, "ptable")
  ptab <- paste0(paste0(dimnames(ptab)$x, ": ", round(ptab, 2)*100, "%"), collapse = " ")
  
  cxtp <- print_context(x)
  
  cat(ptab %+% "\n" %+%
      cxtp,
      sep = "")
  
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
