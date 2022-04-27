#' Non-negative continuous vectors
#'
#' Constructors and methods for a nonnegative continuous data type.
#' \code{v_continuous_nonneg} and \code{nneg} are synonyms that each create a
#' new \code{v_continuous_nonneg} object subclassed from \code{v_continuous} and
#' \code{vctrs_vctr}. \cr\cr
#' Support: \eqn{\mathbf{R^+}}{the nonnegative Reals}* (plus \code{\link{NA_real_}}) \cr
#' Prototype: \code{\link{double}} \cr
#' \emph{}* - i.e. floating-point number
#'
#' @name v_continuous_nonneg
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_ptype2 vec_data new_vctr vec_assert vec_arith_base
#' @inheritParams v_binary
#' @family stype types
#' @examples
#' # Example data
#' src_dbl <- c(1, 2.5, 4, 5.5, NA_real_)
#'
#' # Constructor for the `v_continuous_nonneq` class. One can also use `nneg`
#' # which is a synonym for the `v_continuous_nonneg` function.
#' v <- v_continuous_nonneg(
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
#' is_continuous_nonneg(v)
#' as_nonneg_continuous(src_dbl)
#' as.character(v)
#' as_canonical(v)
NULL

#' The internal builder of v_continuous_nonneg
#' @noRd
#' @keywords internal

new_continuous_nonneg <- function(x = double(),
                                  .internal_name = character(),
                                  .data_summary = data_summary(),
                                  .context = context(),
                                  .auto_compute_summary = auto_compute_default,
                                  .extra_descriptors = list()) {
  vctrs::vec_assert(vctrs::vec_data(x), ptype = double())

  assertthat::assert_that(
    all(x[!is.na(x)] >= 0),
    msg = "Continuous non-negative data must be >= 0"
  )

  new_stype_vctr(
    x,
    .internal_name = .internal_name,
    .data_summary  = .data_summary,
    .context       = .context,
    .auto_compute_summary = .auto_compute_summary,
    .extra_descriptors = .extra_descriptors,
    .class = c("v_continuous_nonneg", "v_continuous")
  )
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_continuous_nonneg", "v_continuous", "vctrs_vctr"))

#' Non-negative continuous constructor
#' @rdname v_continuous_nonneg
#' @param x nonnegative \code{double}
#' @export
v_continuous_nonneg <- make_stype_constructor(
  typeFUN = new_continuous_nonneg,
  ptypeFUN = double,
  dataFUN = vctrs::vec_data
)

#' @rdname v_continuous_nonneg
#' @export
nneg <- v_continuous_nonneg

#' Predicate function for count objects
#' @rdname v_continuous_nonneg
#' @export
is_continuous_nonneg <- function(x) {
  inherits(x, "v_continuous_nonneg")
}

#' @rdname v_continuous_nonneg
#' @export
is_nonneg <- function(x) {
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

#' @method vec_ptype2.v_continuous_nonneg v_continuous_nonneg
#' @export
vec_ptype2.v_continuous_nonneg.v_continuous_nonneg <-
  make_stype_ptype2(v_continuous_nonneg)

#' @method vec_ptype2.v_continuous_nonneg double
#' @export
vec_ptype2.v_continuous_nonneg.double <- function(x, y, ...) x

#' @method vec_ptype2.v_continuous_nonneg v_continuous
#' @export
vec_ptype2.v_continuous_nonneg.v_continuous <- function(x, y, ...) x

#' @rdname casting
#' @method vec_cast v_continuous_nonneg
#' @export
#' @export vec_cast.v_continuous_nonneg
vec_cast.v_continuous_nonneg <- function(x, to, ...) UseMethod("vec_cast.v_continuous_nonneg")

#' @method vec_cast.v_continuous_nonneg v_continuous_nonneg
#' @export
vec_cast.v_continuous_nonneg.v_continuous_nonneg <- function(x, to, ...) x

#' @method vec_cast.v_continuous_nonneg double
#' @export
vec_cast.v_continuous_nonneg.double <- function(x, to, ...) v_continuous_nonneg(x)

#' @importFrom vctrs vec_cast.double
#' @method vec_cast.double v_continuous_nonneg 
#' @export
vec_cast.double.v_continuous_nonneg <- function(x, to, ...) vctrs::vec_data(x)

#' @importFrom vctrs vec_cast.integer
#' @method vec_cast.double v_continuous_nonneg
#' @export
vec_cast.integer.v_continuous_nonneg <- function(x, to, ...) vctrs::vec_data(x)

#' @method vec_cast.v_continuous_nonneg double
#' @export
vec_cast.v_continuous_nonneg.integer <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to)
}

#' Casting function for count objects
#' @rdname v_continuous_nonneg
#' @export
as_nonneg_continuous <- function(x) {
  vctrs::vec_cast(x, new_continuous_nonneg())
}

#' Casting function for count objects
#' @rdname v_continuous_nonneg
#' @export
as_continuous_nonneg <- as_nonneg_continuous

#' @rdname v_continuous_nonneg
#' @export
as_canonical.v_continuous_nonneg <- function(x) {
  as.numeric(vctrs::vec_data(x))
}

# Restoration ####

#' @importFrom vctrs vec_restore
#' @method vec_restore v_continuous_nonneg
#' @export
vec_restore.v_continuous_nonneg <- make_stype_restorator(new_continuous_nonneg)

# Math Operations ####
# NOTE: these strip stype attributes, e.g. context

#' @export
Summary.v_continuous_nonneg <- function(..., na.rm = FALSE) {
  check_summary_args(...)
  vctrs::vec_math(.Generic, vctrs::vec_c(...), na.rm = na.rm)
}

#' @importFrom vctrs vec_math vec_math_base
#' @method vec_math v_continuous_nonneg
#' @export
vec_math.v_continuous_nonneg <- function(.fn, .x, ...) {
  # vec_math.default does not preserve stype class here, so must convert

  switch(.fn,
    # Codomain: v_continuous
    log = v_continuous(vec_math_base(.fn, .x, ...)),

    # Codomain: v_continuous_nonneg
    # not abs = .x since at present we destroy attributes
    cumsum = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),
    cumprod = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),
    cummin = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),
    cummax = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),

    log1p = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),
    abs = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),
    exp = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),
    sqrt = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),

    # Summary
    sum = maybe_get_data_summary_math("sum", .fn, .x, ...),
    mean = maybe_get_data_summary_math("mean", .fn, .x, ...),

    # NOTE: at present vctrs itself does not have an 'invalid math' error type
    stop_invalid_math(.x, .fn)
  )
}


#' @method min v_continuous_nonneg
#' @export
min.v_continuous_nonneg <- make_maybe_get_summary("min", min)

#' @method max v_continuous_nonneg
#' @export
max.v_continuous_nonneg <- make_maybe_get_summary("max", max)


#' @importFrom vctrs vec_arith
#' @method vec_arith v_continuous_nonneg
#' @export
vec_arith.v_continuous_nonneg <- function(op, x, y, ...) {
  UseMethod("vec_arith.v_continuous_nonneg", y)
}

#' @method vec_arith.v_continuous_nonneg default
#' @export
vec_arith.v_continuous_nonneg.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @importFrom vctrs vec_arith_base
#' @method vec_arith.v_continuous_nonneg v_continuous_nonneg
#' @export
vec_arith.v_continuous_nonneg.v_continuous_nonneg <- function(op, x, y, ...) {
  switch(op,
    # Codomain: v_continuous_nonneg
    "+" = v_continuous_nonneg(vec_arith_base(op, x, y)),
    "*" = v_continuous_nonneg(vec_arith_base(op, x, y)),
    "/" = v_continuous_nonneg(vec_arith_base(op, x, y)),
    "^" = v_continuous_nonneg(vec_arith_base(op, x, y)),
    # Codomain: v_continuous
    "-" = v_continuous(vec_arith_base(op, x, y)),
    # Otherwise
    # NOTE: these throw the vctrs error not the stype error
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @importFrom vctrs vec_arith_base
#' @method vec_arith.v_continuous_nonneg v_continuous
#' @export
vec_arith.v_continuous_nonneg.v_continuous <- function(op, x, y, ...) {
  switch(op,
    # Codomain: v_continuous_nonneg
    # TODO: implement ^ for v_continuous x v_continuous_nonneg in the former
    "^" = v_continuous_nonneg(vec_arith_base(op, x, y)),
    # Codomain: v_continuous
    "+" = v_continuous(vec_arith_base(op, x, y)),
    "*" = v_continuous(vec_arith_base(op, x, y)),
    "/" = v_continuous(vec_arith_base(op, x, y)),
    "-" = v_continuous(vec_arith_base(op, x, y)),
    # Otherwise
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.v_continuous_nonneg double
#' @export
vec_arith.v_continuous_nonneg.double <- vec_arith.v_continuous_nonneg.v_continuous

#' @method vec_arith.v_continuous_nonneg integer
#' @export
vec_arith.v_continuous_nonneg.integer <- vec_arith.v_continuous_nonneg.v_continuous

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
  print_footer(
    x,
    stats = list(
      mean = list(label = "Mean", printer = print_numeric_summary),
      variance = list(label = "SD", printer = function(x, label) {
        print_numeric_summary(sqrt(x), label)
      })
    )
  )
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
