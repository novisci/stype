#' Continuous vectors
#' 
#'  Some desc
#' 
#' @name v_continuous
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_type2 vec_data new_vctr vec_assert vec_arith_base
#' @inheritParams v_count
new_continuous <- function(x = double(), .desc = description(), .context = context()){
  x <- vctrs::vec_cast(x, double())
  vctrs::vec_assert(x, ptype = double())
  vctrs::new_vctr(x, desc = .desc, context = .context, class = "v_continuous")
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_continuous", "vctrs_vctr"))

#' Count constructor
#' 
#' constructor function for count objects
#' @rdname v_continuous 
#' @param x a \code{double} vector
#' @export

v_continuous <- function(x = double(), context){
  x <- vctrs::vec_cast(x, double())
  desc <- describe(vctrs::vec_data(x))
  if(missing(context)){
    context <- methods::new("context")
  }
  new_continuous(x, .desc = desc, .context = context)
}

#' Predicate function for count objects
#' @rdname v_continuous 
#' @export

is_continuous <- function(x){
  inherits(x, "v_continuous")
}

# Formatting of example vectors

format.v_continuous <- function(x, ...) {
  ## TODO
  x
}

# Casting and coercing ####

#' @method vec_type2 v_continuous
#' @export
#' @export vec_type2.v_continuous
vec_type2.v_continuous <- function(x, y, ...) UseMethod("vec_type2.v_continuous", y)

#' @method vec_type2.v_continuous default
#' @export
vec_type2.v_continuous.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_type2 double
#' @export
#' @export vec_type2.double
vec_type2.double <- function(x, y, ...) UseMethod("vec_type2.double", y)

#' @method vec_type2.double default
#' @export
vec_type2.double.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @method vec_type2.v_continuous vctrs_unspecified
#' @export
vec_type2.v_continuous.vctrs_unspecified <- function(x, y, ...) x

#' @method vec_type2.v_continuous v_continuous
#' @export
vec_type2.v_continuous.v_continuous <- function(x, y, ...) new_continuous()

#' @method vec_type2.v_continuous double
#' @export
vec_type2.v_continuous.double <- function(x, y, ...) x

#' @method vec_type2.double v_continuous
# @importFrom vctrs vec_type2.double 
#' @export 
vec_type2.double.v_continuous <- function(x, y, ...) y

#' @method vec_cast v_continuous
#' @export
#' @export vec_cast.v_continuous
vec_cast.v_continuous <- function(x, to, ...) UseMethod("vec_cast.v_continuous")

#' @method vec_cast.v_continuous v_continuous
#' @export
vec_cast.v_continuous.v_continuous <- function(x, to, ...) x

#' @method vec_cast.v_continuous default
#' @export
vec_cast.v_continuous.default  <- function(x, to, ...) vctrs::vec_default_cast(x, to)

#' @method vec_cast.v_continuous double
#' @export
vec_cast.v_continuous.double <- function(x, to, ...) v_continuous(x)
vec_cast.double.v_continuous <- function(x, to, ...) vctrs::vec_data(x)

#' @method vec_cast.v_continuous double
#' @export
vec_cast.v_continuous.double <- function(x, to, ...) v_continuous(x)
vec_cast.double.v_continuous <- function(x, to, ...) vctrs::vec_data(x)

#' Casting function for count objects
#' @rdname v_continuous 
#' @export
as_continuous <- function(x) {
  vctrs::vec_cast(x, new_continuous())
}

# Restoration ####
#' @importFrom vctrs vec_restore
#' @method vec_restore v_continuous
#' @export
vec_restore.v_continuous <- function(x, to, ..., i = NULL) {
  # Update description
  desc    <- describe(vctrs::vec_data(x))
  # Maintain context
  context <- get_context(to)
  
  new_continuous(x, .desc = desc, .context = context)
}

# Math Operations ####

#' @importFrom vctrs vec_arith
#' @method vec_arith v_continuous
#' @export
#' @export vec_arith.v_continuous

vec_arith.v_continuous <- function(op, x, y) {
  UseMethod("vec_arith.v_continuous", y)
}

#' @method vec_arith.v_continuous default
#' @export
vec_arith.v_continuous.default <- function(op, x, y) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @method vec_arith.v_continuous v_continuous
#' @export
vec_arith.v_continuous.v_continuous <- function(op, x, y) {
  switch(
    op,
    "+" = v_continuous(vctrs::vec_arith_base(op, x, y)),
    "-" = v_continuous(vctrs::vec_arith_base(op, x, y)),
    "/" = v_continuous(vctrs::vec_arith_base(op, x, y)),
    "*" = v_continuous(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.v_continuous double
#' @export
vec_arith.v_continuous.double <- function(op, x, y) {
  switch(
    op,
    "+" = v_continuous(vctrs::vec_arith_base(op, x, y)),
    "-" = v_continuous(vctrs::vec_arith_base(op, x, y)),
    "/" = v_continuous(vctrs::vec_arith_base(op, x, y)),
    "*" = v_continuous(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith double
#' @export
#' @export vec_arith.double
vec_arith.double <- function(op, x, y) {
  UseMethod("vec_arith.double", y)
}

#' @method vec_arith.double v_continuous
#' @export
vec_arith.double.v_continuous <- function(op, x, y) {
  switch(
    op,
    "+" = new_continuous(vctrs::vec_arith_base(op, x, y)),
    "-" = new_continuous(vctrs::vec_arith_base(op, x, y)),
    "/" = new_continuous(vctrs::vec_arith_base(op, x, y)),
    "*" = new_continuous(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @importFrom vctrs vec_math vec_math_base
#' @method vec_math v_continuous
#' @export
#' @export vec_math.v_continuous
vec_math.v_continuous <- function(fun, x, ...) {
  # TODO implement methods...
  switch(fun,
         sum  = attr(x, "desc")$sum,
         mean = attr(x, "desc")$mean,
         vctrs::vec_math_base(fun, x, ...)
  )
}

#' @importFrom stats median
#' @method median v_continuous
#' @export
median.v_continuous <- function(x, na.rm = FALSE, ...) {
  stats::median(vctrs::vec_data(x), na.rm, ...)
}

#' @importFrom stats quantile
#' @method quantile v_continuous
#' @export
quantile.v_continuous <- function(x, ...) {
  stats::quantile(vctrs::vec_data(x),  ...)
}

# Formatting ####
#' @method format v_continuous
#' @export
format.v_continuous<- function(x, ...) {
  out <- formatC(signif(vctrs::vec_data(x), 3))
  out[is.na(x)] <- NA
  out
}

# Print foot
#' @importFrom vctrs obj_print_footer
#' @method obj_print_footer v_continuous
#' @export
obj_print_footer.v_continuous <- function(x, ...) {
  cat("# Mean: ", attr(x, "desc")[["mean"]], "\n", sep = "")
}

#' @importFrom vctrs vec_ptype_full
#' @method vec_ptype_full v_continuous
#' @export
vec_ptype_full.v_continuous <- function(x) {
  "continuous"
}

#' @importFrom pillar type_sum
#' @export
type_sum.v_continuous <- function(x) {
  "cont"
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr v_continuous
#' @export
vec_ptype_abbr.v_continuous <- function(x) {
  "cont"
}