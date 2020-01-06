#' Binary vectors
#' 
#' @description {
#' Support: \eqn{0, 1}{\{0, 1\}} (plus \code{\link{NA}})
#' 
#' Prototype: \code{\link{logical}}
#' }
#' 
#' @name v_binary
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_ptype2 vec_data new_vctr vec_assert vec_arith_base
#' @inheritParams v_count
#' @family stype types

NULL

#' The internal builder of v_binary
#' @noRd
#' @keywords internal

new_binary <- function(x = logical(),                     
                       .internal_name = character(), 
                       .data_summary = data_summary(), 
                       .context = context()){
  x <- vctrs::vec_cast(x, logical())
  vctrs::vec_assert(x, ptype = logical())
  
  vctrs::new_vctr(
    x, 
    internal_name = .internal_name,
    data_summary  = .data_summary, 
    context       = .context, 
    class = "v_binary")
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_binary", "vctrs_vctr"))

#' Binary constructor
#' @param x a \code{logical} vector
#' @rdname v_binary 
#' @export

v_binary <- function(x = logical(), internal_name = "", context){
  x <- vctrs::vec_cast(x, logical())
  dsum <- describe(vctrs::vec_data(x))
  if(missing(context)){
    context <- methods::new("context")
  }
  new_binary(
    x, 
   .internal_name = internal_name,
   .data_summary  = dsum,
   .context       = context)
}

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

#' @method vec_ptype2.v_binary default
#' @export
vec_ptype2.v_binary.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.v_binary vctrs_unspecified
#' @export
vec_ptype2.v_binary.vctrs_unspecified <- function(x, y, ...) x

#' @method vec_ptype2.v_binary v_binary
#' @export
vec_ptype2.v_binary.v_binary <- function(x, y, ...){
  compare_contexts(x, y)
  v_binary(context = get_context(x))
}

#' @method vec_ptype2.v_binary logical
#' @export
vec_ptype2.v_binary.logical <- function(x, y, ...) { x } 

#' @method vec_ptype2.logical v_binary
#' @importFrom vctrs vec_ptype2.logical
#' @export 
vec_ptype2.logical.v_binary <- function(x, y, ...) { y }

#' @rdname casting
#' @method vec_cast v_binary
#' @export
#' @export vec_cast.v_binary
vec_cast.v_binary <- function(x, to, ...) UseMethod("vec_cast.v_binary")

#' @method vec_cast.v_binary v_binary
#' @export
vec_cast.v_binary.v_binary <- function(x, to, ...) {
  v_binary(vctrs::vec_data(x), context = get_context(to))
}

#' @method vec_cast.v_binary default
#' @export
vec_cast.v_binary.default  <- function(x, to, ...) vctrs::vec_default_cast(x, to)

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
  vctrs::vec_cast(x, new_binary())
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
vec_restore.v_binary <- function(x, to, ..., i = NULL) {
  iname   <- attr(to, "internal_name")
  # Update description
  desc    <- describe(vctrs::vec_data(x))
  # Maintain context
  context <- get_context(to)
  
  new_binary(x, 
             .internal_name = iname, 
             .data_summary  = desc,
             .context       = context)
}

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

## TODO: what are the appropriate math ops for binary data?

#' @method vec_arith.v_binary v_binary
#' @export
vec_arith.v_binary.v_binary <- function(op, x, y) {
  switch(
    op,
    # "+" = new_binary(vctrs::vec_arith_base(op, x, y)),
    # "-" = new_binary(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.v_binary integer
#' @export
vec_arith.v_binary.integer <- function(op, x, y) {
  switch(
    op,
    # "+" = new_binary(vctrs::vec_arith_base(op, x, y)),
    # "-" = new_binary(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith integer
#' @export
#' @export vec_arith.integer
vec_arith.integer <- function(op, x, y) {
  UseMethod("vec_arith.integer", y)
}

#' @method vec_arith.integer v_binary
#' @export
vec_arith.integer.v_binary <- function(op, x, y) {
  switch(
    op,
    # "+" = new_binary(vctrs::vec_arith_base(op, x, y)),
    # "-" = new_binary(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @rdname vec_math
#' @importFrom vctrs vec_math vec_math_base
#' @method vec_math v_binary
#' @export
#' @export vec_math.v_binary
vec_math.v_binary <- function(fun, x, ...) {
  # TODO implement methods...
  switch(fun,
         mean = get_data_summary(x, "proportion"),
         vctrs::vec_math_base(fun, x, ...)
  )
}

#' @method sum v_binary
#' @export
# @export sum.v_binary
sum.v_binary <- function(..., na.rm = TRUE) {
  
  dots <- list(...)
  assertthat::assert_that(
    length(dots) == 1,
    msg = "sum for v_binary only works on one vector at type."
  )
  
  get_data_summary(dots[[1]], "num_1")
}

# @method count v_binary
# @export
# count.v_binary <- function(..., na.rm = TRUE) {
#   sum(..., na.rm = na.rm)
# }

#' @method ! v_binary
#' @export
'!.v_binary' <- function(x){
  v_binary(!vctrs::vec_data(x), context = get_context(x))
}

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
  footer_printer(x, c(proportion = "Proportion"))
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