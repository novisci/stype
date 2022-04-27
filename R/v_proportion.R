#' Proportion vectors
#'
#' Constructors and methods for a proportion  data type.
#' \code{v_proportion} and \code{prop} are synonyms that each create a
#' new \code{v_proportion} object subclassed from \code{v_continuous} and
#' \code{vctrs_vctr}. \cr\cr
#' Support: \eqn{\mathbf{[0, 1]}}* (plus \code{\link{NA_real_}}) \cr
#' Prototype: \code{\link{double}} \cr
#' \emph{}* - i.e. floating-point number
#'
#' @name v_proportion
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_ptype2 vec_data new_vctr vec_assert vec_arith_base
#' @inheritParams v_binary
#' @family stype types
#' @examples
#' # Example data
#' src_dbl <- c(1, 0.5, .01, 0, NA_real_)
#'
#' # Constructor for the `v_proportion` class. One can also use `prop`
#' # which is a synonym for the `v_proportion` function.
#' v <- v_proportion(
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
#' is_proportion(v)
#' as_proportion(src_dbl)
#' as.character(v)
#' as_canonical(v)
NULL

#' The internal builder of v_proportion
#' @noRd
#' @keywords internal

new_proportion <- function(x = double(),
                            .internal_name = character(),
                            .data_summary = data_summary(),
                            .context = context(),
                            .auto_compute_summary = auto_compute_default,
                            .extra_descriptors = list()) {
  vctrs::vec_assert(vctrs::vec_data(x), ptype = double())

  assertthat::assert_that(
    all(x[!is.na(x)] >= 0 & x[!is.na(x)] <= 1),
    msg = "Propotion data must be >= 0 and <= 1"
  )

  new_stype_vctr(
    x,
    .internal_name = .internal_name,
    .data_summary  = .data_summary,
    .context       = .context,
    .auto_compute_summary = .auto_compute_summary,
    .extra_descriptors = .extra_descriptors,
    .class = c("v_proportion", "v_continuous_nonneg", "v_continuous")
  )
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_proportion", "v_continuous_nonneg", "v_continuous", "vctrs_vctr"))

#' Non-negative continuous constructor
#' @rdname v_proportion
#' @param x nonnegative \code{double}
#' @export
v_proportion <- make_stype_constructor(
  typeFUN = new_proportion,
  ptypeFUN = double,
  dataFUN = vctrs::vec_data
)

#' @rdname v_proportion
#' @export
prop <- v_proportion

#' Predicate function for count objects
#' @rdname v_proportion
#' @export
is_proportion <- function(x) {
  inherits(x, "v_proportion")
}

#' @rdname v_proportion
#' @export
is_prop <- function(x) {
  inherits(x, "v_proportion")
}

# Casting and coercing ####
#' @rdname casting
#' @method vec_ptype2 v_proportion
#' @export
#' @export vec_ptype2.v_proportion
vec_ptype2.v_proportion <- function(x, y, ...) UseMethod("vec_ptype2.v_proportion", y)

#' @method vec_ptype2.v_proportion v_proportion
#' @export
vec_ptype2.v_proportion.v_proportion <-
  make_stype_ptype2(v_proportion)

#' @method vec_ptype2.v_proportion double
#' @export
vec_ptype2.v_proportion.double <- function(x, y, ...) x

#' @method vec_ptype2.v_proportion v_continuous
#' @export
vec_ptype2.v_proportion.v_continuous <- function(x, y, ...) x

#' @method vec_ptype2.v_proportion v_continuous_nonneg
#' @export
vec_ptype2.v_proportion.v_continuous_nonneg <- function(x, y, ...) x

#' @rdname casting
#' @method vec_cast v_proportion
#' @export
#' @export vec_cast.v_proportion
vec_cast.v_proportion <- function(x, to, ...) UseMethod("vec_cast.v_proportion")

#' @method vec_cast.v_proportion v_proportion
#' @export
vec_cast.v_proportion.v_proportion <- function(x, to, ...) x

#' @method vec_cast.v_proportion double
#' @export
vec_cast.v_proportion.double <- function(x, to, ...) v_proportion(x)
vec_cast.double.v_proportion <- function(x, to, ...) vctrs::vec_data(x)

#' Casting function for count objects
#' @rdname v_proportion
#' @export
as_proportion <- function(x) {
  vctrs::vec_cast(x, new_proportion())
}

#' @rdname v_proportion
#' @export
as_canonical.v_proportion <- function(x) {
  as.numeric(vctrs::vec_data(x))
}

# Restoration ####

#' @importFrom vctrs vec_restore
#' @method vec_restore v_proportion
#' @export
vec_restore.v_proportion <- make_stype_restorator(new_proportion)

# Math Operations ####
# NOTE: these strip stype attributes, e.g. context

#' @importFrom vctrs vec_math vec_math_base
#' @method vec_math v_proportion
#' @export
vec_math.v_proportion <- function(.fn, .x, ...) {
  # vec_math.default does not preserve stype class here, so must convert

  switch(.fn,
    # Codomain: v_continuous
    log = v_continuous(vec_math_base(.fn, .x, ...)),

    # Codomain: v_continuous_nonneg
    # not abs = .x since at present we destroy attributes
    cumsum = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),
    cumprod = v_proportion(vec_math_base(.fn, .x, ...)),
    cummin = v_proportion(vec_math_base(.fn, .x, ...)),
    cummax = v_proportion(vec_math_base(.fn, .x, ...)),

    log1p = v_continuous(vec_math_base(.fn, .x, ...)),
    abs = v_proportion(vec_math_base(.fn, .x, ...)),
    exp = v_continuous(vec_math_base(.fn, .x, ...)),
    sqrt = v_proportion(vec_math_base(.fn, .x, ...)),

    # Summary
    sum = maybe_get_data_summary_math("sum", .fn, .x, ...),
    mean = maybe_get_data_summary_math("mean", .fn, .x, ...),

    # NOTE: at present vctrs itself does not have an 'invalid math' error type
    stop_invalid_math(.x, .fn)
  )
}

#' @method min v_proportion 
#' @export
min.v_proportion <- make_maybe_get_summary("min", min)

#' @method max v_proportion
#' @export
max.v_proportion <- make_maybe_get_summary("max", max)

#' @importFrom vctrs vec_arith
#' @method vec_arith v_proportion
#' @export
vec_arith.v_proportion <- function(op, x, y, ...) {
  UseMethod("vec_arith.v_proportion", y)
}

#' @method vec_arith.v_proportion default
#' @export
vec_arith.v_proportion.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @importFrom vctrs vec_arith_base
#' @method vec_arith.v_proportion v_proportion
#' @export
vec_arith.v_proportion.v_proportion <- function(op, x, y, ...) {
  switch(op,
    # Codomain: v_proportion
    "*" = v_proportion(vec_arith_base(op, x, y)),
    "^" = v_proportion(vec_arith_base(op, x, y)),
    
    # Codomain: v_continuous_nonneg
    "+" = v_continuous_nonneg(vec_arith_base(op, x, y)),
    "/" = v_continuous_nonneg(vec_arith_base(op, x, y)),
    
    # Codomain: v_continuous
    "-" = v_continuous(vec_arith_base(op, x, y)),
    # Otherwise
    # NOTE: these throw the vctrs error not the stype error
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @importFrom vctrs vec_arith_base
#' @method vec_arith.v_proportion v_continuous
#' @export
vec_arith.v_proportion.v_continuous <- function(op, x, y, ...) {
  switch(op,
    # Codomain: v_continuous
    "^" = v_continuous(vec_arith_base(op, x, y)),
    "+" = v_continuous(vec_arith_base(op, x, y)),
    "*" = v_continuous(vec_arith_base(op, x, y)),
    "/" = v_continuous(vec_arith_base(op, x, y)),
    "-" = v_continuous(vec_arith_base(op, x, y)),
    # Otherwise
    vctrs::stop_incompatible_op(op, x, y)
  )
}


# Formatting ####
#' @method format v_proportion 
#' @export
format.v_proportion <- function(x, ...) {
  out <- formatC(signif(vctrs::vec_data(x), 3))
  out[is.na(x)] <- NA
  out
}

# Print foot
#' @importFrom vctrs obj_print_footer
#' @method obj_print_footer v_proportion
#' @export
obj_print_footer.v_proportion <- function(x, ...) {
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
#' @method vec_ptype_full v_proportion
#' @export
vec_ptype_full.v_proportion <- function(x) {
  "proportion"
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr v_proportion
#' @export
vec_ptype_abbr.v_proportion <- function(x) {
  "prop"
}

#' @importFrom pillar type_sum
#' @export
type_sum.v_proportion <- function(x) {
  "prop"
}
