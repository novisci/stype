#' Count vectors
#' 
#'  Some desc
#' 
#' @name v_count
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_type2 vec_data new_vctr vec_assert vec_arith_base

new_count <- function(x = integer(), .desc = description(), .context = context()){
  vctrs::vec_assert(x, ptype = integer())
  assertthat::assert_that(
    all(x[!is.na(x)] >= 0),
    msg = "Count data must be >= 0"
  )
  # vctrs::vec_assert(desc, ptype = description())
  
  vctrs::new_vctr(x, desc = .desc, context = .context, class = "v_count")
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_count", "vctrs_vctr"))

#' Count constructor
#' 
#' constructor function for count objects
#' @rdname v_count 
#' @export

v_count <- function(x = integer(), context, ...){
  x <- vctrs::vec_cast(x, integer())
  desc <- describe(vctrs::vec_data(x))
  if(missing(context)){
    context <- methods::new("context")
  }
  new_count(x, .desc = desc, .context = context)
}

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

#' @method vec_type2 v_count
#' @export
#' @export vec_type2.v_count
vec_type2.v_count <- function(x, y, ...) UseMethod("vec_type2.v_count", y)

#' @method vec_type2.v_count default
#' @export
vec_type2.v_count.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_type2.v_count vctrs_unspecified
#' @export
vec_type2.v_count.vctrs_unspecified <- function(x, y, ...) x

#' @method vec_type2.v_count v_count
#' @export
vec_type2.v_count.v_count <- function(x, y, ...) new_count()

#' @method vec_type2.v_count integer
#' @export
vec_type2.v_count.integer <- function(x, y, ...) x

#' @method vec_type2.integer v_count
#' @importFrom vctrs vec_type2.integer 
#' @export 
vec_type2.integer.v_count <- function(x, y, ...) y

#' @method vec_cast v_count
#' @export
#' @export vec_cast.v_count
vec_cast.v_count <- function(x, to) UseMethod("vec_cast.v_count")

#' @method vec_cast.v_count v_count
#' @export
vec_cast.v_count.v_count <- function(x, to) x

#' @method vec_cast.v_count default
#' @export
vec_cast.v_count.default  <- function(x, to) vctrs::vec_default_cast(x, to)

#' @method vec_cast.v_count integer
#' @export
vec_cast.v_count.integer <- function(x, y, ...) v_count(x)
vec_cast.integer.v_count <- function(x, y, ...) vctrs::vec_data(x)

#' @method vec_cast.v_count numeric
#' @export
vec_cast.v_count.numeric <- function(x, y, ...) v_count(x)
vec_cast.numeric.v_count <- function(x, y, ...) vctrs::vec_data(x)

#' Casting function for count objects
#' @rdname v_count 
#' @export
as_count <- function(x) {
  vctrs::vec_cast(x, new_count())
}

# Restoration ####
#' @importFrom vctrs vec_restore
#' @method vec_restore v_count
#' @export
vec_restore.v_count <- function(x, to, ..., i = NULL) {
  # Update description
  desc    <- describe(vctrs::vec_data(x))
  # Maintain context
  context <- get_context(to)
  
  new_count(x, .desc = desc, .context = context)
}

# Math Operations ####

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
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.v_count integer
#' @export
vec_arith.v_count.integer <- function(op, x, y) {
  switch(
    op,
    "+" = v_count(vctrs::vec_arith_base(op, x, y)),
    "-" = v_count(vctrs::vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}

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
    stop_incompatible_op(op, x, y)
  )
}

#' @importFrom vctrs vec_math vec_math_base
#' @method vec_math v_count
#' @export
#' @export vec_math.v_count
vec_math.v_count <- function(fun, x, ...) {
  # TODO implement methods...
  switch(fun,
         sum  = attr(x, "desc")$sum,
         mean = attr(x, "desc")$mean,
         vctrs::vec_math_base(fun, x, ...)
  )
}

#' @importFrom stats median
#' @method median v_count
#' @export
median.v_count <- function(x, na.rm = FALSE, ...) {
  median(vctrs::vec_data(x), na.rm, ...)
}

#' @importFrom stats quantile
#' @method quantile v_count
#' @export
quantile.v_count <- function(x, ...) {
  quantile(vctrs::vec_data(x),  ...)
}

#' @method range v_count
#' @export
range.v_count <- function(..., na.rm = FALSE) {
  range(vctrs::vec_data(..1),  na.rm = FALSE)
}

# Formatting ####

# format.v_count<- function(x, ...) {
#   out <- formatC(signif(vec_data(x) * 100, 3))
#   out[is.na(x)] <- NA
#   out[!is.na(x)] <- paste0(out[!is.na(x)], "%")
#   out
# }

# Print foot
#' @importFrom vctrs obj_print_footer
#' @method obj_print_footer v_count
#' @export
obj_print_footer.v_count <- function(x, ...) {
  cat("# Total: ", attr(x, "desc")[["sum"]], "\n", sep = "")
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
#' @export
type_sum.v_count <- function(x) {
  "cnt"
}