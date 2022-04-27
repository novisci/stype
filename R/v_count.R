#' Count vectors
#'
#' Constructors and methods for a count data type. \code{v_count} and
#' \code{cnt} are synonyms that each create a new \code{v_count} object
#' subclassed from \code{vctrs_vctr}. \cr\cr
#' Support: \eqn{\mathbf{N^+}}{the non-negative Integers}* (plus \code{\link{NA_integer_}}) \cr
#' Prototype: \code{\link{integer}} \cr
#' \emph{}* - i.e. the natural numbers including 0
#'
#' @name v_count
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_ptype2 vec_data new_vctr vec_assert vec_arith_base
#' @inheritParams v_binary
#' @family stype types
#' @examples
#' # Example data
#' src_count <- seq_len(5L)
#'
#' # Constructor for the `v_count` class. One can also use `cnt` which is a
#' # synonym for the `v_count` function.
#' v <- v_count(
#'   x = src_count,
#'   internal_name = "v_example",
#'   context = context(
#'     short_label = "important_var",
#'     long_label  = "Very important variable"
#'   ),
#'   extra_descriptors = list()
#' )
#'
#' # Helper functions and methods
#' is_count(v)
#' as_count(src_count)
#' as.character(v)
#' as_canonical(v)
NULL

#' The internal builder of v_count
#' @noRd
#' @param .internal_name the internal name of the variable
#' @param .data_summary a \code{\link{data_summary}}
#' @param .context a \code{\link{context}}
#' @param .extra_descriptors A \code{list} of \code{\link{descriptors}} functions
#'        appended to the default \code{\link{descriptors}}.
#' @keywords internal

new_count <- function(x = integer(),
                      .internal_name = character(),
                      .data_summary = data_summary(),
                      .context = context(),
                      .auto_compute_summary = auto_compute_default,
                      .extra_descriptors = list()) {

  # x <- vctrs::vec_cast(x, integer())
  vctrs::vec_assert(vctrs::vec_data(x), ptype = integer())

  assertthat::assert_that(
    all(x[!is.na(x)] >= 0),
    msg = "Count data must be >= 0"
  )

  new_stype_vctr(
    x,
    .internal_name = .internal_name,
    .data_summary  = .data_summary,
    .context       = .context,
    .auto_compute_summary = .auto_compute_summary,
    .extra_descriptors = .extra_descriptors,
    .class = c("v_count")
  )
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_count", "vctrs_vctr"))

#' Count constructor
#' @rdname v_count
#' @export
v_count <- make_stype_constructor(
  typeFUN  = new_count,
  ptypeFUN = integer,
  castFUN  = integer,
  dataFUN  = vctrs::vec_data
)

#' @rdname v_count
#' @export
cnt <- v_count

#' Predicate function for count objects
#' @rdname v_count
#' @export
is_count <- function(x) {
  inherits(x, "v_count")
}

# Formatting of example vectors

format.v_count <- function(x, ...) {
  ## TODO
  x
}

# Casting and coercing ####

#' Casting
#' @name casting
#' @inheritParams vctrs::vec_ptype2
#' @method vec_ptype2 v_count
#' @export
#' @export vec_ptype2.v_count
vec_ptype2.v_count <- function(x, y, ...) UseMethod("vec_ptype2.v_count", y)

#' @method vec_ptype2.v_count v_count
#' @export
vec_ptype2.v_count.v_count <- make_stype_ptype2(v_count)

#' @method vec_ptype2.v_count integer
#' @export
vec_ptype2.v_count.integer <- function(x, y, ...) x

#' @method vec_ptype2.v_count double
#' @export
vec_ptype2.v_count.double <- function(x, y, ...) x

#' @rdname casting
#' @inheritParams vctrs::vec_cast
#' @method vec_cast v_count
#' @export
#' @export vec_cast.v_count
vec_cast.v_count <- function(x, to, ...) UseMethod("vec_cast.v_count")

#' @method vec_cast.v_count v_count
#' @export
vec_cast.v_count.v_count <- function(x, to, ...) x

#' @method vec_cast.v_count integer
#' @export
vec_cast.v_count.integer <- function(x, to, ...) v_count(x)
vec_cast.integer.v_count <- function(x, to, ...) vctrs::vec_data(x)

#' @method vec_cast.v_count double
#' @export
vec_cast.v_count.double <- function(x, to, ...) v_count(x)
vec_cast.double.v_count <- function(x, to, ...) vctrs::vec_data(x)

#' Casting function for count objects
#' @rdname v_count
#' @export
as_count <- function(x) {
  vctrs::vec_cast(x, new_count())
}

#' @rdname v_count
#' @export
as.character.v_count <- function(x, ...) {
  as.character(as_canonical(x))
}

#' @rdname v_count
#' @export
as_canonical <- function(x) {
  UseMethod("as_canonical")
}

#' @rdname v_count
#' @export
as_canonical.v_count <- function(x) {
  as.integer(vctrs::vec_data(x))
}

# Restoration ####
#' @importFrom vctrs vec_restore
#' @method vec_restore v_count
#' @export
vec_restore.v_count <- make_stype_restorator(new_count)

# Math Operations ####
#' Arithmetic ops
#' @name vec_arith
#' @inheritParams vctrs::vec_arith
#' @importFrom vctrs vec_arith
#' @method vec_arith v_count
#' @export
#' @export vec_arith.v_count

vec_arith.v_count <- function(op, x, y) {
  UseMethod("vec_arith.v_count", y)
}

#' @rdname vec_arith
#' @method vec_arith integer
#' @export
#' @export vec_arith.integer
vec_arith.integer <- function(op, x, y) {
  UseMethod("vec_arith.integer", y)
}


#' @method vec_arith.v_count default
#' @export
vec_arith.v_count.default <- function(op, x, y) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @method vec_arith.v_count v_count
#' @export
vec_arith.v_count.v_count <- function(op, x, y) {
  switch(op,
    "+" = v_count(vctrs::vec_arith_base(op, x, y)),
    "-" = vctrs::vec_arith_base(op, x, y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.v_count integer
#' @export
vec_arith.v_count.integer <- function(op, x, y) {
  switch(op,
    # Codomain: v_count
    "+" = v_count(vctrs::vec_arith_base(op, x, y)),
    "*" = v_count(vctrs::vec_arith_base(op, x, y)),
    "^" = v_count(vctrs::vec_arith_base(op, x, y)),

    # Codomain: integer
    "-" = vctrs::vec_arith_base(op, x, y),

    # Codomain: v_continuous
    "/" = v_continuous(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.integer v_count
#' @export
vec_arith.integer.v_count <- function(op, x, y) {
  switch(op,
    # Codomain: v_count
    "+" = v_count(vctrs::vec_arith_base(op, x, y)),
    "*" = v_count(vctrs::vec_arith_base(op, x, y)),
    "^" = v_count(vctrs::vec_arith_base(op, x, y)),

    # Codomain: integer
    "-" = vctrs::vec_arith_base(op, x, y),

    # Codomain: v_continuous
    "/" = v_continuous(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @export
Summary.v_count <- function(..., na.rm = FALSE) {
  check_summary_args(...)
  vctrs::vec_math(.Generic, vctrs::vec_c(...), na.rm = na.rm)
}

#' Math
#' @name vec_math
#' @inheritParams vctrs::vec_math
#' @importFrom vctrs vec_math vec_math_base
#' @method vec_math v_count
#' @export
#' @export vec_math.v_count
vec_math.v_count <- function(.fn, .x, ...) {
  switch(.fn,

    ## Math
    # Domain: v_count
    cumsum = v_count(vec_math_base(.fn, .x, ...)),
    cumprod = v_count(vec_math_base(.fn, .x, ...)),
    cummin = v_count(vec_math_base(.fn, .x, ...)),
    cummax = v_count(vec_math_base(.fn, .x, ...)),

    # Codomain: v_continuous_nonneg
    log = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),
    log1p = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),
    abs = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),
    exp = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),
    sqrt = v_continuous_nonneg(vec_math_base(.fn, .x, ...)),

    ## Summary
    sum = maybe_get_data_summary_math("sum", .fn, .x, ...),
    mean = maybe_get_data_summary_math("mean", .fn, .x, ...),
    stop_invalid_math(.x, .fn)
  )
}

#' @method min v_count
#' @export
min.v_count <- make_maybe_get_summary("min", min)

#' @method max v_count
#' @export
max.v_count <- make_maybe_get_summary("max", max)

#' @importFrom stats median
#' @method median v_count
#' @export
median.v_count <- function(x, na.rm = FALSE, ...) {
  stats::median(vctrs::vec_data(x), na.rm, ...)
}

#' @importFrom stats quantile
#' @method quantile v_count
#' @export
quantile.v_count <- function(x, ...) {
  stats::quantile(vctrs::vec_data(x), ...)
}

#' @method range v_count
#' @export
range.v_count <- function(..., na.rm = FALSE) {
  range(vctrs::vec_data(..1), na.rm = FALSE)
}

# Formatting ####
#' @method format v_count
#' @export
format.v_count <- function(x, ...) {
  out <- vctrs::vec_data(x)
  out[is.na(x)] <- NA_integer_
  out
}

#' @importFrom vctrs obj_print_footer
#' @method obj_print_footer v_count
#' @export
obj_print_footer.v_count <- function(x, ...) {
  print_footer(
    x,
    stats = list(
      sum = list(
        label = "Total",
        printer = function(x, label) {
          print_numeric_summary(x, label, "%d")
        }
      ),
      mean = list(label = "Mean", printer = print_numeric_summary)
    )
  )
}

#' @importFrom vctrs vec_ptype_full
#' @method vec_ptype_full v_count
#' @export
vec_ptype_full.v_count <- function(x) {
  "count"
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr v_count
#' @export
vec_ptype_abbr.v_count <- function(x) {
  "cnt"
}

#' @importFrom pillar type_sum
#' @method type_sum v_count
#' @export
type_sum.v_count <- function(x) {
  "cnt"
}
