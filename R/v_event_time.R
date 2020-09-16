#' Time to event vectors
#' 
#' @description  {
#' Support: \eqn{\mathbf{R^+}}{the nonnegative Reals}* (plus \code{\link{NA_real_}})
#' 
#' Prototype: \code{\link{double}}
#' 
#' * - i.e. floating-point number
#' }
#' 
#' @name v_event_time
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_ptype2 vec_data new_vctr vec_assert vec_arith_base
#' @inheritParams v_count
#' @family stype types
NULL

#' The internal builder of v_event_time
#' @noRd
#' @keywords internal
new_event_time <- function(x = double(), 
                           .internal_name = character(), 
                           .data_summary = data_summary(), 
                           .context = context(),
                           .extra_descriptors = list()){
  
  vctrs::vec_assert(vctrs::vec_data(x), ptype = double())
  assertthat::assert_that(
    all(x[!is.na(x)] >= 0),
    msg = "Time to event data must be >= 0."
  )
  # vctrs::vec_assert(desc, ptype = description())
  
  vctrs::new_vctr(
    x, 
    internal_name = .internal_name,
    data_summary  = .data_summary, 
    context       = .context,
    extra_descriptors = .extra_descriptors,
    class = c("v_event_time", "v_continuous_nonneg", "v_continuous"))
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_event_time", "v_continuous_nonneg",
                       "v_continuous", "vctrs_vctr"))

#' Time to event constructor
#' @param x vector of values
#' @rdname v_event_time 
#' @export
v_event_time <- make_stype_constructor(
  typeFUN = new_event_time,
  ptypeFUN = v_continuous_nonneg,
  dataFUN  = vctrs::vec_data
)

#' @rdname v_event_time 
#' @export
tmev <- v_event_time

#' Predicate function for count objects
#' @rdname v_event_time 
#' @export
is_event_time <- function(x){
  inherits(x, "v_event_time")
}

# Formatting of example vectors
format.v_event_time <- function(x, ...) {
  ## TODO
  x
}

# Casting and coercing ####
#' @rdname casting
#' @method vec_ptype2 v_event_time
#' @export
#' @export vec_ptype2.v_event_time
vec_ptype2.v_event_time <- function(x, y, ...) UseMethod("vec_ptype2.v_event_time", y)

#' @method vec_ptype2.v_event_time v_event_time
#' @export
vec_ptype2.v_event_time.v_event_time <- function(x, y, ...) {
  compare_contexts(x, y)
  check_internal_names(x, y)
  
  v_event_time(
    internal_name = get_internal_name(x),
    context = get_context(x))
}

#' @method vec_ptype2.v_event_time double
#' @export
vec_ptype2.v_event_time.double <- function(x, y, ...) x

#' @method vec_ptype2.v_event_time v_continuous
#' @export
vec_ptype2.v_event_time.v_continuous <- function(x, y, ...) x

#' @method vec_ptype2.v_event_time v_continuous_nonneg
#' @export
vec_ptype2.v_event_time.v_continuous_nonneg <- function(x, y, ...) x

#' @rdname casting
#' @method vec_cast v_event_time
#' @export
#' @export vec_cast.v_event_time
vec_cast.v_event_time <- function(x, to, ...) UseMethod("vec_cast.v_event_time")

#' @method vec_cast.v_event_time v_event_time
#' @export
vec_cast.v_event_time.v_event_time <- function(x, to, ...) x

#' @method vec_cast.v_event_time numeric
#' @export
vec_cast.v_event_time.numeric <- function(x, to, ...) v_event_time(x)
vec_cast.numeric.v_event_time <- function(x, to, ...) vctrs::vec_data(x)

#' @method vec_cast.v_event_time numeric
#' @export
vec_cast.v_event_time.numeric <- function(x, to, ...) v_event_time(x)
vec_cast.numeric.v_event_time <- function(x, to, ...) vctrs::vec_data(x)

#' Casting function for time to event objects
#' @rdname v_event_time 
#' @export
as_time_to_event <- function(x) {
  vctrs::vec_cast(x, new_event_time())
}

#' @rdname v_event_time 
#' @export
as_event_time <- function(x) {
  vctrs::vec_cast(x, new_event_time())
}

#' @rdname v_event_time
#' @export
as_canonical.v_event_time<- function(x){
  as.numeric(vctrs::vec_data(x))
}

# Restoration ####
#' @importFrom vctrs vec_restore
#' @method vec_restore v_event_time
#' @export
vec_restore.v_event_time <- make_stype_restorator(new_event_time)

# Math Operations ####
# TODO: ?

# Formatting ####
#' @method format v_event_time
#' @export
format.v_event_time <- function(x, ...) {
  out <- formatC(signif(vctrs::vec_data(x), 3))
  out[is.na(x)] <- NA
  out
}

# Print foot
#' @importFrom vctrs obj_print_footer
#' @method obj_print_footer v_event_time
#' @export
obj_print_footer.v_event_time <- function(x, ...) {
  print_footer(x, c(median = "Median survival"))
}

#' @importFrom vctrs vec_ptype_full
#' @method vec_ptype_full v_event_time
#' @export
vec_ptype_full.v_event_time <- function(x) {
  "time-to-event"
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr v_event_time
#' @export
vec_ptype_abbr.v_event_time <- function(x) {
  "tmev"
}

#' @importFrom pillar type_sum
#' @export
type_sum.v_event_time <- function(x) {
  "tmev"
}
