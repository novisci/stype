#' Binary vectors
#' 
#' Constructors and methods for a binary data type. \code{v_binary} and
#' \code{bnry} are synonyms that each create a new \code{v_binary} object
#' subclassed from \code{vctrs_vctr}. \cr\cr
#' Support: \eqn{\{0, 1\}} (plus \code{\link{NA}}) \cr
#' Prototype: \code{\link{logical}}
#'
#' @name v_binary
#' @param x a \code{integer} vector
#' @param internal_name the internal name of the variable
#' @param context a \code{\link{context}}
#' @param auto_compute_summary an indicator of whether the \code{data_summary} is
#'    automatically computed whenever a vector is initialized, subset, or 
#'    concatenated. Defaults to \code{TRUE}. If this option is set to \code{FALSE},
#'    then \code{\link{get_data_summary}} is the only way to compute the summary. 
#'    The \code{\link{data_summary_l}} lens will return an empty \code{data_summary}.
#' @param ... passed to other methods such as \code{as.character}
#' @param extra_descriptors A \code{list} of \code{\link{descriptors}} functions
#'        appended to the default \code{\link{descriptors}}.
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_ptype2 vec_data new_vctr vec_assert vec_arith_base
#' @importFrom utils packageVersion
#' @family stype types
#' @examples
#' # Example data
#' src_binary <- c(TRUE, FALSE, TRUE, FALSE, NA)
#'
#' # Constructor for the `v_binary` class. One can also use `bnry` which is a
#' # synonym for the `v_binary` function.
#' v <- v_binary(
#'   x = src_binary,
#'   internal_name = "v_example",
#'   context = context(
#'     short_label = "important_var",
#'     long_label  = "Very important variable"
#'   ),
#'   extra_descriptors = list()
#' )
#'
#' # Helper functions and methods
#' is_binary(v)
#' as_binary(src_binary)
#' as.character(v)
#' as_canonical(v)
NULL

#' The internal builder of v_binary
#' @noRd
#' @keywords internal
new_binary <- function(x = logical(),                     
                       .internal_name = character(), 
                       .data_summary = data_summary(), 
                       .context = context(),
                       .auto_compute_summary = auto_compute_default,
                       .extra_descriptors = list()){

  x <- vctrs::vec_cast(x, logical())
  vctrs::vec_assert(x, ptype = logical())
  
  new_stype_vctr(
    x, 
    .internal_name = .internal_name,
    .data_summary  = .data_summary, 
    .context       = .context, 
    .auto_compute_summary = .auto_compute_summary,
    .extra_descriptors = .extra_descriptors,
    .class = "v_binary")
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_binary", "vctrs_vctr"))

#' Binary constructor
#' @param x a \code{logical} vector or any vector that can be cast to a
#'   \code{logical} vector via \code{\link[vctrs:vec_cast]{vctrs::vec_cast()}}
#'   such as \code{integer} or \code{numeric} vectors with values in \{0, 1\}.
#' @rdname v_binary
#' @export
v_binary <- make_stype_constructor(
  typeFUN = new_binary,
  ptypeFUN = logical,
  castFUN  = logical,
  dataFUN  = vctrs::vec_data
)

#' @rdname v_binary 
#' @export
bnry <- v_binary

#' Predicate function for binary objects
#' @rdname v_binary 
#' @export
is_binary <- function(x){
  inherits(x, "v_binary")
}

# Casting and coercing ####
#' @rdname casting
#' @method vec_ptype2 v_binary
#' @export
#' @export vec_ptype2.v_binary
vec_ptype2.v_binary <- function(x, y, ...) UseMethod("vec_ptype2.v_binary", y)

#' @method vec_ptype2.v_binary v_binary
#' @export
vec_ptype2.v_binary.v_binary <- make_stype_ptype2(v_binary)

#' @method vec_ptype2.v_binary logical
#' @export
vec_ptype2.v_binary.logical <- function(x, y, ...)  x  

#' @rdname casting
#' @method vec_cast v_binary
#' @export
#' @export vec_cast.v_binary
vec_cast.v_binary <- function(x, to, ...) UseMethod("vec_cast.v_binary")

#' @method vec_cast.v_binary v_binary
#' @export
vec_cast.v_binary.v_binary <- function(x, to, ...) x 

#' @method vec_cast.v_binary logical
#' @export
vec_cast.v_binary.logical <- function(x, to, ...) v_binary(x)

#' @importFrom vctrs vec_cast.logical
#' @method vec_cast.logical v_binary
#' @export
vec_cast.logical.v_binary <- function(x, to, ...) vctrs::vec_data(x)

#' Casting function for binary objects
#' @rdname v_binary 
#' @export
as_binary <- function(x) {
  x <- vctrs::vec_cast(x, logical())
  vctrs::vec_cast(x, new_binary())
}

#' @rdname v_binary 
#' @export
as.character.v_binary <- function(x, ...) {
  ifelse(vctrs::vec_data(x), "1", "0")
}

#' @rdname v_binary 
#' @export
as_canonical.v_binary <- function(x){
  as.logical(vctrs::vec_data(x))
}

# Restoration ####
#' @importFrom vctrs vec_restore
#' @method vec_restore v_binary
#' @export
vec_restore.v_binary <- make_stype_restorator(new_binary)

# Math Operations ####

#' @rdname vec_arith
#' @importFrom vctrs vec_arith
#' @method vec_arith v_binary
#' @export
#' @export vec_arith.v_binary

vec_arith.v_binary <- function(op, x, y) {
  UseMethod("vec_arith.v_binary", y)
}

#' @method vec_arith.v_binary default
#' @export
vec_arith.v_binary.default <- function(op, x, y) {
  vctrs::stop_incompatible_op(op, x, y)
}

# vctrs implements methods for each of `+`, `-`, `*`, `/`, `^`, `%%`, `%/%`,
# `!`, `&`, and `|` for objects that subclass `vctrs_vctr` such that each of
# these methods call the `vec_arith` generic. We then implement a stype method
# for `vec_arith` for the type defined in this file that itself is a generic
# and for arguments of the appropriate type in the second position dispatch to
# this method. If we were not to do this then the `vec_arith` generic will fall
# through to vctrs implementation which effectively calls the operator in the
# base package for the data contained in the two arguments for situations that
# make sense and throws an error otherwise.
#' @method vec_arith.v_binary v_binary
#' @export
vec_arith.v_binary.v_binary <- function(op, x, y) {
  switch(
    op,
    "*" = v_binary(x & y),
    "+" = v_binary(x | y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}


#' The internal builder of v_binary x other arithmetic
#' @noRd
#' @keywords internal

build_arith_vec_binary <- function(func){
  function(op, x, y) {
    switch(
      op,
      "^" = func(vctrs::vec_arith_base(op, x, as.double(as_canonical(y)))),
      "*" = func(vctrs::vec_arith_base(op, x, as.double(as_canonical(y)))),
      vctrs::stop_incompatible_op(op, x, y)
    )
  }
}

#' The internal builder of other x v_binary arithmetic
#' @noRd
#' @keywords internal

build_arith_binary_vec <- function(func){
  function(op, x, y) {
    switch(
      op,
      "*" = func(vctrs::vec_arith_base(op, as.double(as_canonical(x)), y)),
      "^" = func(vctrs::vec_arith_base(op, as.double(as_canonical(x)), y)),
      vctrs::stop_incompatible_op(op, x, y)
    )
  }
}

#' @method vec_arith.v_binary integer
#' @export
vec_arith.v_binary.integer <- build_arith_binary_vec(as.integer)


#' @method vec_arith.integer v_binary
#' @export
vec_arith.integer.v_binary <- build_arith_vec_binary(as.integer)

#' @method vec_arith.v_binary double
#' @export
vec_arith.v_binary.double <- build_arith_binary_vec(as.double)


#' @method vec_arith.double v_binary
#' @export
vec_arith.double.v_binary <- build_arith_vec_binary(as.double)

#' @method vec_arith.v_binary v_continuous
#' @export
vec_arith.v_binary.v_continuous <- build_arith_binary_vec(v_continuous)


#' @method vec_arith.v_continuous v_binary
#' @export
vec_arith.v_continuous.v_binary <- build_arith_vec_binary(v_continuous)

#' @method vec_arith.v_binary v_continuous_nonneg
#' @export
vec_arith.v_binary.v_continuous_nonneg <- build_arith_binary_vec(v_continuous_nonneg)


#' @method vec_arith.v_continuous_nonneg v_binary
#' @export
vec_arith.v_continuous_nonneg.v_binary <- build_arith_vec_binary(v_continuous_nonneg)

#' @method vec_arith.v_binary v_count
#' @export
vec_arith.v_binary.v_count <- build_arith_binary_vec(v_count)


#' @method vec_arith.v_count v_binary
#' @export
vec_arith.v_count.v_binary <- build_arith_vec_binary(v_count)

#' Math Operations
#'
#' Math operations implementations. Note that these strip stype attributes, e.g.
#' context.
#'
#' vctrs implements a `Math` method which has the effect of any math operator
#' being called for a subclasses of `vctrs_vct` being dispatched to
#' `vctrs::Math.vctrs_vctr`. That method then calls the generic
#' `vctrs::vec_math` so by creating a method for this class we prevent the
#' dispatch from falling through to `vec_math.default`. We need to create our
#' own definitions for these math operators since `vec_math.default` has the
#' effect of invoking the appropriate operator in the base package with the raw
#' data as the input (for vctrs types that are supported), and then restoring
#' the appropriate vctrs type but not stype type (for vctrs types that are not
#' supported an error is thrown immediately).
#'
#' Further note that vctrs also implements `Summary.vctrs_vctr` which routes to
#' the `vctrs::vec_math` generic so we can include operators in that group in
#' this function as well.
#'
#' See the `groupGeneric` documentation or
#' https://adv-r.hadley.nz/s3.html#group-generics for more details.
#'
#' @rdname vec_math
#' @importFrom vctrs vec_math vec_math_base
#' @method vec_math v_binary
#' @export
#' @export vec_math.v_binary
vec_math.v_binary <- function(.fn, .x, ...) {
  switch(.fn,
         # Domain: v_count
         cumsum = v_count(vec_math_base(.fn, .x, ...)),
         
         # Domain: v_binary  
         cumprod = v_binary(vec_math_base(.fn, .x, ...)),
         cummin = v_binary(vec_math_base(.fn, .x, ...)),
         cummax = v_binary(vec_math_base(.fn, .x, ...)),
         
         # Summary
         mean = maybe_get_data_summary_math("proportion", .fn, .x, ...),
         sum  = maybe_get_data_summary_math("num_1", .fn, .x, ...),
         
         stop_invalid_math(.x, .fn)
  )
}

# The Summary method is included in order to handle the ... arguments of calls to
# `sum` using the check_summary_args utility.
#' @export
Summary.v_binary <- function(..., na.rm = FALSE) {
  check_summary_args(...)
  vctrs::vec_math(.Generic, vctrs::vec_c(...), na.rm = na.rm)
}

#' @method ! v_binary
#' @export
'!.v_binary' <- function(x){
  v_binary(
    !vctrs::vec_data(x),
    context = get_context(x),
    auto_compute_summary = view(x, auto_compute_summary_l),
    extra_descriptors    = view(x, extra_descriptors_l)
  )
}

#' @method | v_binary
#' @export
'|.v_binary' <- function(x, y){
  
  assertthat::assert_that(
    length(x) == length(y) || length(x) == 1L || length(y) == 1L,
    msg = "x and y must have the same size or either x or y should be a scalar."
  )
  
  x <- vctrs::vec_cast(x, to = logical())
  y <- vctrs::vec_cast(y, to = logical())
  x | y
}

#' @method & v_binary
#' @export
'&.v_binary' <- function(x, y){
  
  assertthat::assert_that(
    length(x) == length(y) || length(x) == 1L || length(y) == 1L,
    msg = "x and y must have the same size or either x or y should be a scalar."
  )
  
  x <- vctrs::vec_cast(x, to = logical())
  y <- vctrs::vec_cast(y, to = logical())
  x & y 
}

# @method * v_binary
# @export
# '*.v_binary' <- function(x, y){ x & y }

#' @method all v_binary
#' @export
all.v_binary <- function(..., na.rm = TRUE) {
  purrr::lift_dv(all)(vctrs::vec_data(..1), na.rm = na.rm)
}

#' @method any v_binary
#' @export
any.v_binary <- function(..., na.rm = TRUE) {
  purrr::lift_dv(any)(vctrs::vec_data(..1), na.rm = na.rm)
}

# Formatting ####
#' @method format v_binary
#' @export
format.v_binary <- function(x, ...) {
  # Display as 0/1
  out <- as.integer(vctrs::vec_data(x))
  out[is.na(x)] <- NA
  out
}

# Print foot
#' @importFrom vctrs obj_print_footer
#' @method obj_print_footer v_binary
#' @export
obj_print_footer.v_binary <- function(x, ...) {
  print_footer(
    x, 
    stats = list(
      proportion = list(label = "Proportion", printer = print_numeric_summary))
    )
}

#' @importFrom vctrs vec_ptype_full
#' @method vec_ptype_full v_binary
#' @export
vec_ptype_full.v_binary <- function(x) {
  "binary"
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr v_binary
#' @export
vec_ptype_abbr.v_binary <- function(x) {
  "bnry"
}

#' @importFrom pillar type_sum
#' @export
type_sum.v_binary <- function(x) {
  "bnry"
}
