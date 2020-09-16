#' Count vectors
#' 
#' @description  {
#' Support: \eqn{\mathbf{N^+}}{the non-negative Integers}* (plus \code{\link{NA_integer_}})
#' 
#' Prototype: \code{\link{integer}}
#' }
#' 
#' @name v_count
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_ptype2 vec_data new_vctr vec_assert vec_arith_base
#' @param x a \code{integer} vector
#' @param internal_name the internal name of the variable
#' @param context a \code{\link{context}}
#' @param ... passed to other methods such as \code{as.character}
#' @param extra_descriptors A \code{list} of \code{\link{descriptors}} functions
#'        appended to the default \code{\link{descriptors}}.
#' @family stype types
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
                      .extra_descriptors = list()){
  
  # x <- vctrs::vec_cast(x, integer())
  vctrs::vec_assert(vctrs::vec_data(x), ptype = integer())
  
  assertthat::assert_that(
    all(x[!is.na(x)] >= 0),
    msg = "Count data must be >= 0"
  )
  
  vctrs::new_vctr(
    x, 
    internal_name = .internal_name,
    data_summary  = .data_summary, 
    context       = .context, 
    extra_descriptors = .extra_descriptors,
    class         = "v_count")
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
  dataFUN  = vctrs::vec_data)

#' @rdname v_count 
#' @export
cnt <- v_count

#' Predicate function for count objects
#' @rdname v_count 
#' @export
is_count <- function(x){
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
vec_ptype2.v_count.v_count <- function(x, y, ...) {
  compare_contexts(x, y)
  check_internal_names(x, y)
  
  v_count(internal_name = get_internal_name(x), 
          context = get_context(x))
} 

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
as.character.v_count <- function(x, ...){
  as.character(as_canonical(x))
}

#' @rdname v_count 
#' @export
as_canonical <- function(x){
  UseMethod("as_canonical")
}

#' @rdname v_count 
#' @export
as_canonical.v_count <- function(x){
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

#' @method vec_arith.v_count default
#' @export
vec_arith.v_count.default <- function(op, x, y) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @method vec_arith.v_count v_count
#' @export
vec_arith.v_count.v_count <- function(op, x, y) {
  switch(
    op,
    "+" = v_count(vctrs::vec_arith_base(op, x, y)),
    "-" = v_count(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.v_count integer
#' @export
vec_arith.v_count.integer <- function(op, x, y) {
  switch(
    op,
    "+" = v_count(vctrs::vec_arith_base(op, x, y)),
    "-" = v_count(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @rdname vec_arith
#' @method vec_arith integer
#' @export
#' @export vec_arith.integer
vec_arith.integer <- function(op, x, y) {
  UseMethod("vec_arith.integer", y)
}

#' @method vec_arith.integer v_count
#' @export
vec_arith.integer.v_count <- function(op, x, y) {
  switch(
    op,
    "+" = v_count(vctrs::vec_arith_base(op, x, y)),
    "-" = v_count(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' Math
#' @name vec_math
#' @param fun fun
#' @param x x
#' @param ... dots
#' @importFrom vctrs vec_math vec_math_base
#' @method vec_math v_count
#' @export
#' @export vec_math.v_count
vec_math.v_count <- function(fun, x, ...) {
  # TODO implement methods...
  switch(fun,
         sum  = get_data_summary(x, "sum"),
         mean = get_data_summary(x, "mean"),
         vctrs::vec_math_base(fun, x, ...)
  )
}

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
  stats::quantile(vctrs::vec_data(x),  ...)
}

#' @method range v_count
#' @export
range.v_count <- function(..., na.rm = FALSE) {
  range(vctrs::vec_data(..1),  na.rm = FALSE)
}

# Formatting ####
#' @method format v_count
#' @export
format.v_count<- function(x, ...) {
  out <- vctrs::vec_data(x)
  out[is.na(x)] <- NA_integer_
  out
}

# @importFrom vctrs obj_print_header
# @method obj_print_header v_count
# @export
# obj_print_header.v_count <- function(x, ...) {
#   cat(standard_header(x))
# }

#' @importFrom vctrs obj_print_footer
#' @method obj_print_footer v_count
#' @export
obj_print_footer.v_count <- function(x, ...) {
  print_footer(x, c(sum = "Total", mean = "Mean"))
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
