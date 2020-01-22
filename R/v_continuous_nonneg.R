#' Non-negative continuous vectors
#' 
#' @description  {
#' Support: \eqn{\mathbf{R^+}}{the nonnegative Reals}* (plus \code{\link{NA_real_}})
#' 
#' Prototype: \code{\link{double}}
#' 
#' * - i.e. floating-point number
#' }
#' 
#' @name v_continuous_nonneg
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_ptype2 vec_data new_vctr vec_assert vec_arith_base
#' @inheritParams v_count
#' @family stype types
NULL

#' The internal builder of v_continuous_nonneg
#' @noRd
#' @keywords internal

new_continuous_nonneg <- function(x = double(), 
                                  .internal_name = character(), 
                                  .data_summary = data_summary(), 
                                  .context = context()){
  
  # x <- vctrs::vec_cast(x, double())
  vctrs::vec_assert(vctrs::vec_data(x), ptype = double())
  
  assertthat::assert_that(
    all(x[!is.na(x)] >= 0),
    msg = "Continuous non-negative data must be >= 0"
  )
  
  vctrs::new_vctr(
    x,
    internal_name = .internal_name,
    data_summary  = .data_summary, 
    context       = .context, 
    class = c("v_continuous_nonneg", "v_continuous"))
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_continuous_nonneg", "v_continuous", "vctrs_vctr"))

#' Non-negative continuous constructor
#' @rdname v_continuous_nonneg 
#' @param x nonnegative \code{double}
#' @export

v_continuous_nonneg <- function(x = double(), internal_name = "", context){
  # x <- vctrs::vec_cast(x, double())
  dsum <- describe(vctrs::vec_data(x))
  if(missing(context)){
    context <- methods::new("context")
  }
  new_continuous_nonneg(
    x,
    .internal_name = internal_name,
    .data_summary  = dsum,
    .context       = context)
}

#' Predicate function for count objects
#' @rdname v_continuous_nonneg 
#' @export

is_continuous_nonneg <- function(x){
  inherits(x, "v_continuous_nonneg")
}


#' @rdname v_continuous_nonneg 
#' @export

is_nonneg <- function(x){
  inherits(x, "v_continuous_nonneg")
}

# Formatting of example vectors

format.v_continuous_nonneg <- function(x, ...) {
  ## TODO
  x
}

# Casting and coercing ####
#' @rdname casting
#' @method vec_ptype2 v_continuous_nonneg
#' @export
#' @export vec_ptype2.v_continuous_nonneg
vec_ptype2.v_continuous_nonneg <- function(x, y, ...) UseMethod("vec_ptype2.v_continuous_nonneg", y)

#' @method vec_ptype2.v_continuous_nonneg default
#' @export
vec_ptype2.v_continuous_nonneg.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @rdname casting
#' @method vec_ptype2 numeric
#' @export
#' @export vec_ptype2.numeric
vec_ptype2.numeric <- function(x, y, ...) UseMethod("vec_ptype2.numeric", y)

#' @method vec_ptype2.numeric default
#' @export
vec_ptype2.numeric.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @method vec_ptype2.v_continuous_nonneg vctrs_unspecified
#' @export
vec_ptype2.v_continuous_nonneg.vctrs_unspecified <- function(x, y, ...) x

#' @method vec_ptype2.v_continuous_nonneg v_continuous_nonneg
#' @export
vec_ptype2.v_continuous_nonneg.v_continuous_nonneg <- function(x, y, ...) new_continuous_nonneg()

#' @method vec_ptype2.v_continuous_nonneg numeric
#' @export
vec_ptype2.v_continuous_nonneg.numeric <- function(x, y, ...) x

#' @method vec_ptype2.numeric v_continuous_nonneg
# @importFrom vctrs vec_ptype2.numeric 
#' @export 
vec_ptype2.numeric.v_continuous_nonneg <- function(x, y, ...) y

#' @rdname casting
#' @method vec_cast v_continuous_nonneg
#' @export
#' @export vec_cast.v_continuous_nonneg
vec_cast.v_continuous_nonneg <- function(x, to, ...) UseMethod("vec_cast.v_continuous_nonneg")

#' @method vec_cast.v_continuous_nonneg v_continuous_nonneg
#' @export
vec_cast.v_continuous_nonneg.v_continuous_nonneg <- function(x, to, ...) x

#' @method vec_cast.v_continuous_nonneg default
#' @export
vec_cast.v_continuous_nonneg.default  <- function(x, to, ...) vctrs::vec_default_cast(x, to)

#' @method vec_cast.v_continuous_nonneg numeric
#' @export
vec_cast.v_continuous_nonneg.numeric <- function(x, to, ...) v_continuous_nonneg(x)
vec_cast.numeric.v_continuous_nonneg <- function(x, to, ...) vctrs::vec_data(x)

#' @method vec_cast.v_continuous_nonneg numeric
#' @export
vec_cast.v_continuous_nonneg.numeric <- function(x, to, ...) v_continuous_nonneg(x)
vec_cast.numeric.v_continuous_nonneg <- function(x, to, ...) vctrs::vec_data(x)

#' Casting function for count objects
#' @rdname v_continuous_nonneg 
#' @export
as_nonneg_continuous <- function(x) {
  vctrs::vec_cast(x, new_continuous_nonneg())
}

#' @rdname v_continuous_nonneg
#' @export
as_canonical.v_continuous_nonneg <- function(x){
  as.numeric(vctrs::vec_data(x))
}

# Restoration ####
#' @importFrom vctrs vec_restore
#' @method vec_restore v_continuous_nonneg
#' @export
vec_restore.v_continuous_nonneg <- function(x, to, ..., i = NULL) {
  
  iname   <- attr(to, "internal_name")
  # Update description
  desc    <- describe(vctrs::vec_data(x))
  # Maintain context
  context <- get_context(to)
  
  new_continuous_nonneg(
    x,
    .internal_name = iname,
    .data_summary  = desc, 
    .context = context)
}

# Math Operations ####
# TODO:?

# Formatting ####
#' @method format v_continuous_nonneg
#' @export
format.v_continuous_nonneg <- function(x, ...) {
  out <- formatC(signif(vctrs::vec_data(x), 3))
  out[is.na(x)] <- NA
  out
}
# Print foot
#' @importFrom vctrs obj_print_footer
#' @method obj_print_footer v_continuous_nonneg
#' @export
obj_print_footer.v_continuous_nonneg <- function(x, ...) {
  print_footer(x, c(mean = "Mean", sd = "SD"))
}

#' @importFrom vctrs vec_ptype_full
#' @method vec_ptype_full v_continuous_nonneg
#' @export
vec_ptype_full.v_continuous_nonneg <- function(x) {
  "continuous nonnegative"
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr v_continuous_nonneg
#' @export
vec_ptype_abbr.v_continuous_nonneg <- function(x) {
  "nneg"
}

#' @importFrom pillar type_sum
#' @export
type_sum.v_continuous_nonneg <- function(x) {
  "nneg"
}
