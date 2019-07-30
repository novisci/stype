#' v_ordered S3 class
#' 
#' An ordered (categorial) variable
#' 
#' @name v_ordered
#' @param .levels Character vector of labels.
#' @inheritParams v_count

new_ordered <- function(x = integer(), .levels = character(),
                        .internal_name = character(), 
                        .data_summary = data_summary(), 
                        .context = context()) {
  # browser()
  stopifnot(is.integer(x))
  stopifnot(is.character(.levels))
  
  structure(
    x,
    levels  = .levels,
    internal_name = .internal_name,
    data_summary  = .data_summary, 
    context       = .context,
    class   = c("v_ordered", "vctrs_vctr", "ordered", "factor")
  )
}
#' @importFrom methods setOldClass
methods::setOldClass(c("v_ordered", "vctrs_vctr"))

#' Construction for a v_ordered
#' @rdname v_ordered
#' @param x a \code{factor}
#' @export
v_ordered <- function(x = factor(ordered = TRUE), internal_name = "", context){
  # x <- vctrs::vec_cast(x, factor())
  dsum <- describe(x)
  if(missing(context)){
    context <- methods::new("context")
  }
  
  new_ordered(
    x        = as.integer(x),
    .levels  = levels(x), 
    .internal_name = internal_name,
    .data_summary  = dsum,
    .context       = context)
}

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

#' @method vec_ptype2 double
#' @export
#' @export vec_ptype2.double
vec_ptype2.character <- function(x, y, ...) UseMethod("vec_ptype2.character", y)

#' @method vec_ptype2.v_ordered default
#' @export
vec_ptype2.v_ordered.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}
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
  new_ordered(.levels =  union(levels(x),levels(y)),
              .context = get_context(x))
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
#' @method vec_cast.v_ordered default
vec_cast.v_ordered.default <- function(x, to, ..., x_arg = "", to_arg = "") {
  vctrs::vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
#' @method vec_restore v_ordered 
vec_restore.v_ordered <- function(x, to, ..., x_arg = "", to_arg = "") {
  iname   <- attr(to, "internal_name")
  x   <- levels(to)[x]
  out <- factor(x, levels = levels(to), ordered = TRUE)

  # Maintain context
  ctxt <- get_context(to)
  
  v_ordered(out, internal_name = iname, context = ctxt)
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
  # TODO: use footer_printer
  ptab <- attr(x, "data_summary")[["ptable"]]
  ptab <- paste0(paste0(dimnames(ptab)$x, ": ", round(ptab, 2)*100, "%"), collapse = " ")
  
  cxtp <- context_printer(x)
  
  cat(ptab %+% "\n" %+%
        cxtp,
      sep = "")
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
