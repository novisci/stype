#' Continuous vectors
#' 
#' @description  {
#' Support: \eqn{\mathbf{R}}{the Reals}* (plus \code{\link{NA_real_}})
#' 
#' Prototype: \code{\link{double}}
#' 
#' * - i.e. floating-point number
#' }
#' 
#' @name v_continuous
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_ptype2 vec_data new_vctr vec_assert vec_arith_base
#' vec_ptype2.double
#' @inheritParams v_count
#' @family stype types
NULL

#' The internal builder of v_continuous
#' @noRd
#' @keywords internal

new_continuous <- function(x = double(),
                           .internal_name = character(), 
                           .data_summary = data_summary(), 
                           .context = context(),
                           .extra_descriptors = list()){
  
  # x <- vctrs::vec_cast(x, double())
  vctrs::vec_assert(vctrs::vec_data(x), ptype = double())
  
  vctrs::new_vctr(
    x,
    internal_name = .internal_name,
    data_summary  = .data_summary, 
    context       = .context, 
    extra_descriptors = .extra_descriptors,
    class = "v_continuous")
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_continuous", "vctrs_vctr"))

#' Count constructor
#' @rdname v_continuous 
#' @param x a \code{double} vector
#' @export
v_continuous <- make_stype_constructor(
  typeFUN = new_continuous,
  ptypeFUN = double,
  dataFUN  = vctrs::vec_data
)

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

#' @rdname casting
#' @method vec_ptype2 v_continuous
#' @export
#' @export vec_ptype2.v_continuous
vec_ptype2.v_continuous <- function(x, y, ...) UseMethod("vec_ptype2.v_continuous", y)

#' @method vec_ptype2.v_continuous v_continuous
#' @export
vec_ptype2.v_continuous.v_continuous <- function(x, y, ...) {
  compare_contexts(x, y)
  check_internal_names(x, y)
  
  v_continuous(
    internal_name = get_internal_name(x),
    context = get_context(x))
}

#' @method vec_ptype2.double v_continuous
#' @export
vec_ptype2.double.v_continuous <- function(x, y, ...) new_continuous()

#' @rdname casting
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
vec_cast.v_continuous.double <- function(x, to, ...) x

vec_cast.double.v_continuous <- function(x, to, ...) vctrs::vec_data(x)

#' Casting function for continuous objects
#' @rdname v_continuous 
#' @export
as_continuous <- function(x) {
  vctrs::vec_cast(x, new_continuous())
}

#' @rdname v_continuous 
#' @export
as.character.v_continuous <- function(x, ...) {
  as.character(as_canonical(x))
}

#' @rdname v_continuous
#' @export
as_canonical.v_continuous <- function(x){
  as.numeric(vctrs::vec_data(x))
}

# Restoration ####
#' @importFrom vctrs vec_restore
#' @method vec_restore v_continuous
#' @export
vec_restore.v_continuous <- make_stype_restorator(new_continuous)

# Math Operations ####

#' @rdname vec_arith
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

#' @rdname vec_arith
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

#' @rdname vec_math
#' @importFrom vctrs vec_math vec_math_base
#' @method vec_math v_continuous
#' @export
#' @export vec_math.v_continuous
vec_math.v_continuous <- function(fun, x, ...) {
  # TODO implement methods...
  switch(fun,
         sum  = get_data_summary(x, "sum"),
         mean = get_data_summary(x, "mean"),
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
format.v_continuous <- function(x, ...) {
  out <- formatC(signif(vctrs::vec_data(x), 3))
  out[is.na(x)] <- NA
  out
}

# Print foot
#' @importFrom vctrs obj_print_footer
#' @method obj_print_footer v_continuous
#' @export
obj_print_footer.v_continuous <- function(x, ...) {
  print_footer(x, c(mean = "Mean", sd = "SD"))
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