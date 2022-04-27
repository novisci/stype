#' Continuous vectors
#' 
#' Constructors and methods for a continuous data type. \code{v_continuous} and
#' \code{cont} are synonyms that each create a new \code{v_continuous} object
#' subclassed from \code{vctrs_vctr}. \cr\cr
#' Support: \eqn{\mathbf{R}}{the Reals}* (plus \code{\link{NA_real_}}) \cr
#' Prototype: \code{\link{double}} \cr
#' \emph{}* - i.e. floating-point number
#'
#' @name v_continuous
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_ptype2 vec_data new_vctr vec_assert vec_arith_base
#' vec_ptype2.double
#' @inheritParams v_binary
#' @family stype types
#' @examples
#' # Example data
#' src_dbl <- c(-0.5, 1, 2.5, 4, 5.5, NA_real_)
#'
#' # Constructor for the `v_continuous` class. One can also use `cont` which is a
#' # synonym for the `v_continuous` function.
#' v <- v_continuous(
#'   x = src_dbl,
#'   internal_name = "v_example",
#'   context = context(
#'     short_label = "important_var",
#'     long_label  = "Very important variable"
#'   ),
#'   extra_descriptors = list()
#' )
#'
#' # Helper functions and methods
#' is_continuous(v)
#' as_continuous(src_dbl)
#' as.character(v)
#' as_canonical(v)
NULL

#' The internal builder of v_continuous
#' @noRd
#' @keywords internal

new_continuous <- function(x = double(),
                           .internal_name = character(), 
                           .data_summary = data_summary(), 
                           .context = context(),
                           .auto_compute_summary = auto_compute_default,
                           .extra_descriptors = list()){
  
  # x <- vctrs::vec_cast(x, double())
  vctrs::vec_assert(vctrs::vec_data(x), ptype = double())
  
  new_stype_vctr(
    x, 
    .internal_name = .internal_name,
    .context       = .context, 
    .data_summary  = .data_summary,
    .auto_compute_summary = .auto_compute_summary,
    .extra_descriptors = .extra_descriptors,
    .class = c("v_continuous"))
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

#' @rdname v_continuous 
#' @export
cont <- v_continuous

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
vec_ptype2.v_continuous.v_continuous <- make_stype_ptype2(v_continuous)

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
vec_cast.v_continuous.double <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to)
}

#' @importFrom vctrs vec_cast.integer
#' @method vec_cast.double v_continuous
#' @export
vec_cast.integer.v_continuous <- function(x, to, ...) vctrs::vec_data(x)

#' @method vec_cast.v_continuous double
#' @export
vec_cast.v_continuous.integer <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to)
}

#' @importFrom vctrs vec_cast.double
#' @method vec_cast.double v_continuous
#' @export
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
    "^" = v_continuous(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.v_continuous v_continuous_nonneg
#' @export
vec_arith.v_continuous.v_continuous_nonneg <- vec_arith.v_continuous.v_continuous

#' @method vec_arith.v_continuous double
#' @export
vec_arith.v_continuous.double <- vec_arith.v_continuous.v_continuous

#' @method vec_arith.v_continuous integer
#' @export
vec_arith.v_continuous.integer <- vec_arith.v_continuous.v_continuous

#' @rdname vec_arith
#' @method vec_arith double
#' @export
#' @export vec_arith.double
vec_arith.double <- function(op, x, y) {
  UseMethod("vec_arith.double", y)
}

#' @method vec_arith.double v_continuous
#' @export
vec_arith.double.v_continuous <- vec_arith.v_continuous.v_continuous

#' @method vec_arith.integer v_continuous
#' @export
vec_arith.integer.v_continuous <- vec_arith.double.v_continuous

#' @export
Summary.v_continuous <- function(..., na.rm = FALSE) {
  check_summary_args(...)
  vctrs::vec_math(.Generic, vctrs::vec_c(...), na.rm = na.rm)
}


#' @rdname vec_math
#' @importFrom vctrs vec_math vec_math_base
#' @method vec_math v_continuous
#' @export
vec_math.v_continuous <- function(.fn, .x, ...) {
  switch(.fn,
         
         ## Math
         # Domain: v_continuous
         cumsum = v_continuous(vec_math_base(.fn, .x, ...)),
         cumprod = v_continuous(vec_math_base(.fn, .x, ...)),
         cummin = v_continuous(vec_math_base(.fn, .x, ...)),
         cummax = v_continuous(vec_math_base(.fn, .x, ...)),
         
         
         # Codomain: v_continuous_nonneg
         abs = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),
         exp = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),

         
         ## Summary
         sum = {
           check_summary_args(...)
           maybe_get_data_summary_math("sum", .fn, .x, ...)
        },
         mean = maybe_get_data_summary_math("mean", .fn, .x, ...),
         stop_invalid_math(.x, .fn)
  )
}

#' @importFrom stats median
#' @method median v_continuous
#' @export
median.v_continuous <- function(x, na.rm = FALSE, ...) {
   `if`(is_auto_computed(x) && na.rm,
        get_data_summary(x, "median"),
        stats::median(vctrs::vec_data(x), na.rm, ...))
 }

#' @method min v_continuous
#' @export
min.v_continuous <- make_maybe_get_summary("min", min)

#' @method max v_continuous
#' @export
max.v_continuous <- make_maybe_get_summary("max", max)

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
  print_footer(
    x, 
    stats = list(mean = list(label = "Mean", printer = print_numeric_summary),
                 variance = list(label = "SD", printer = function(x, label) {
                   print_numeric_summary(sqrt(x), label)
                 }))
    
  )
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
