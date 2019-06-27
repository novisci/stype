#' continuous_nonneg vectors
#' 
#'  Some desc
#' 
#' @name v_continuous_nonneg
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_type2 vec_data new_vctr vec_assert vec_arith_base
#' @inheritParams v_count

new_continuous_nonneg <- function(x = double(), .desc = description(), .context = context()){
  vctrs::vec_assert(x, ptype = double())
  assertthat::assert_that(
    all(x[!is.na(x)] >= 0),
    msg = "Count data must be >= 0"
  )
  # vctrs::vec_assert(desc, ptype = description())
  
  vctrs::new_vctr(x, desc = .desc, context = .context,
                  class = c("v_continuous_nonneg", "v_continuous"))
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_continuous_nonneg", "v_continuous", "vctrs_vctr"))

#' Count constructor
#' 
#' constructor function for count objects
#' @rdname v_continuous_nonneg 
#' @param x nonnegative \code{double}
#' @export

v_continuous_nonneg <- function(x = double(), context){
  x <- vctrs::vec_cast(x, double())
  desc <- describe(vctrs::vec_data(x))
  if(missing(context)){
    context <- methods::new("context")
  }
  new_continuous_nonneg(x, .desc = desc)
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

#' @method vec_type2 v_continuous_nonneg
#' @export
#' @export vec_type2.v_continuous_nonneg
vec_type2.v_continuous_nonneg <- function(x, y, ...) UseMethod("vec_type2.v_continuous_nonneg", y)

#' @method vec_type2.v_continuous_nonneg default
#' @export
vec_type2.v_continuous_nonneg.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_type2 numeric
#' @export
#' @export vec_type2.numeric
vec_type2.numeric <- function(x, y, ...) UseMethod("vec_type2.numeric", y)

#' @method vec_type2.numeric default
#' @export
vec_type2.numeric.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @method vec_type2.v_continuous_nonneg vctrs_unspecified
#' @export
vec_type2.v_continuous_nonneg.vctrs_unspecified <- function(x, y, ...) x

#' @method vec_type2.v_continuous_nonneg v_continuous_nonneg
#' @export
vec_type2.v_continuous_nonneg.v_continuous_nonneg <- function(x, y, ...) new_continuous_nonneg()

#' @method vec_type2.v_continuous_nonneg numeric
#' @export
vec_type2.v_continuous_nonneg.numeric <- function(x, y, ...) x

#' @method vec_type2.numeric v_continuous_nonneg
# @importFrom vctrs vec_type2.numeric 
#' @export 
vec_type2.numeric.v_continuous_nonneg <- function(x, y, ...) y

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

# Restoration ####
#' @importFrom vctrs vec_restore
#' @method vec_restore v_continuous_nonneg
#' @export
vec_restore.v_continuous_nonneg <- function(x, to, ..., i = NULL) {
  # Update description
  desc    <- describe(vctrs::vec_data(x))
  # Maintain context
  context <- get_context(to)
  
  new_continuous_nonneg(x, .desc = desc, .context = context)
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
  cat("# Mean: ", attr(x, "desc")[["mean"]], "\n", sep = "")
}

#' @importFrom vctrs vec_ptype_full
#' @method vec_ptype_full v_continuous_nonneg
#' @export
vec_ptype_full.v_continuous_nonneg <- function(x) {
  "continuous_nonneg"
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