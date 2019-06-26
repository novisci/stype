#' v_nominal/ordered v_nominal S3 class
#'
#' A [v_nominal] is an integer with attribute `levels`, a character vector. There
#' should be one level for each integer between 1 and `max(x)`.
#' An [ordered] v_nominal has the same properties as a v_nominal, but possesses
#' an extra class that marks levels as having a total ordering.
#'
#' These functions help the base v_nominal and ordered v_nominal classes fit in to
#' the vctrs type system by providing constructors, coercion functions,
#' and casting functions. `new_v_nominal()` and `new_ordered()` are low-level
#' constructors - they only check that types, but not values, are valid, so
#' are for expert use only.
#'
#' @param x Integer values which index in to `levels`.
#' @param .levels Character vector of labels.
#' @param .desc a description
#' @param .context a context

new_nominal <- function(x = integer(), .levels = character(),
                        .desc = description(),
                        .context = context()) {
  # browser()
  stopifnot(is.integer(x))
  stopifnot(is.character(.levels))
  
  structure(
    x,
    levels  = .levels,
    context = .context,
    desc    = .desc,
    class   = c("v_nominal", "vctrs_vctr", "factor")
  )
}
#' @importFrom methods setOldClass
methods::setOldClass(c("v_nominal", "vctrs_vctr"))

#' Construction for a v_nominal
#' @rdname v_nominal
#' @export
v_nomimal <- function(x = factor(), context, ...){
  # x <- vctrs::vec_cast(x, factor())
  desc <- describe(x)
  if(missing(context)){
    context <- methods::new("context")
  }
  
  new_nominal(x        = as.integer(x),
              .levels  = levels(x), 
              .desc    = desc, 
              .context = context)
}

# Coerce ------------------------------------------------------------------

#' @rdname new_nominal
#' @export vec_type2.v_nominal
#' @method vec_type2 v_nominal
#' @export
vec_type2.v_nominal <- function(x, y, ...) UseMethod("vec_type2.v_nominal", y)

#' @method vec_type2 double
#' @export
#' @export vec_type2.double
vec_type2.character <- function(x, y, ...) UseMethod("vec_type2.character", y)

#' @method vec_type2.v_nominal default
#' @export
vec_type2.v_nominal.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_type2.character v_nominal
#' @export
vec_type2.character.v_nominal <- function(x, y, ...) character()
#' @method vec_type2.v_nominal character
#' @export
vec_type2.v_nominal.character <- function(x, y, ...) character()
#' @method vec_type2.v_nominal v_nominal
#' @export
vec_type2.v_nominal.v_nominal <- function(x, y, ...) {
  compare_contexts(x, y)
  new_nominal(.levels =  union(levels(x),levels(y)),
              .context = get_context(x))
}

# Cast --------------------------------------------------------------------

#' @rdname new_nominal
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
  vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.v_nominal default
vec_cast.v_nominal.default <- function(x, to, ..., x_arg = "", to_arg = "") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
#' @method vec_restore v_nominal 
vec_restore.v_nominal <- function(x, to, ..., x_arg = "", to_arg = "") {
  x   <- levels(to)[x]
  out <- factor(x, levels = levels(to))

  # Maintain context
  ctxt <- get_context(to)
  
  v_nomimal(out, context = ctxt)
}


#' @export
#' @method levels v_nominal 

levels.v_nominal <- levels.default

# Math and arithmetic -----------------------------------------------------

#' @export
vec_math.v_nominal <- function(.fn, .x, ...) {
  stop_unsupported(.x, .fn)
}

#' @export
vec_arith.v_nominal <- function(op, x, y, ...) {
  stop_unsupported(x, op)
}

# Print -------------------------------------------------------------------

#' @export
vec_ptype_full.v_nominal <- function(x, ...) {
  paste0("nominal<", hash_label(levels(x)), ">")
}

#' @export
vec_ptype_abbr.v_nominal <- function(x, ...) {
  "nom"
}

# Helpers -----------------------------------------------------------------

hash_label <- function(x, length = 5) {
  if (length(x) == 0) {
    ""
  } else {
    # Can't use hash() currently because it hashes the string pointers
    # for performance, so the values in the test change each time
    substr(digest::digest(x), 1, length)
  }
}
