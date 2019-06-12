#' event_time vectors
#' 
#'  Some desc
#' 
#' @name v_event_time
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_type2 vec_data new_vctr vec_assert vec_arith_base

new_event_time <- function(x = double(), .desc = description(), .context = context()){
  vctrs::vec_assert(x, ptype = double())
  assertthat::assert_that(
    all(x[!is.na(x)] >= 0),
    msg = "Time to event data must be >= 0."
  )
  # vctrs::vec_assert(desc, ptype = description())
  
  vctrs::new_vctr(x, desc = .desc, context = .context,
                  class = c("v_event_time", "v_continuous_nonneg", "v_continuous"))
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_event_time", "v_continuous_nonneg", "v_continuous", "vctrs_vctr"))

#' Count constructor
#' 
#' constructor function for count objects
#' @rdname v_event_time 
#' @export

v_event_time <- function(x = v_continuous_nonneg(), context, ...){
  x <- vctrs::vec_cast(x, double())
  desc <- describe(vctrs::vec_data(x))
  if(missing(context)){
    context <- methods::new("context")
  }
  new_event_time(x, .desc = desc, .context = context)
}

#' Predicate function for count objects
#' @rdname v_event_time 
#' @export

is_event_time <- function(x){
  inherits(x, "v_event_time")
}


#' @rdname v_event_time 
#' @export

is_nonneg <- function(x){
  inherits(x, "v_event_time")
}

# Formatting of example vectors

format.v_event_time <- function(x, ...) {
  ## TODO
  x
}

# Casting and coercing ####

#' @method vec_type2 v_event_time
#' @export
#' @export vec_type2.v_event_time
vec_type2.v_event_time <- function(x, y, ...) UseMethod("vec_type2.v_event_time", y)

#' @method vec_type2.v_event_time default
#' @export
vec_type2.v_event_time.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_type2 numeric
#' @export
#' @export vec_type2.numeric
vec_type2.numeric <- function(x, y, ...) UseMethod("vec_type2.numeric", y)

#' @method vec_type2.numeric default
#' @export
vec_type2.numeric.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @method vec_type2.v_event_time vctrs_unspecified
#' @export
vec_type2.v_event_time.vctrs_unspecified <- function(x, y, ...) x

#' @method vec_type2.v_event_time v_event_time
#' @export
vec_type2.v_event_time.v_event_time <- function(x, y, ...) new_event_time()

#' @method vec_type2.v_event_time numeric
#' @export
vec_type2.v_event_time.numeric <- function(x, y, ...) x

#' @method vec_type2.numeric v_event_time
# @importFrom vctrs vec_type2.numeric 
#' @export 
vec_type2.numeric.v_event_time <- function(x, y, ...) y

#' @method vec_cast v_event_time
#' @export
#' @export vec_cast.v_event_time
vec_cast.v_event_time <- function(x, to) UseMethod("vec_cast.v_event_time")

#' @method vec_cast.v_event_time v_event_time
#' @export
vec_cast.v_event_time.v_event_time <- function(x, to) x

#' @method vec_cast.v_event_time default
#' @export
vec_cast.v_event_time.default  <- function(x, to) vctrs::vec_default_cast(x, to)

#' @method vec_cast.v_event_time numeric
#' @export
vec_cast.v_event_time.numeric <- function(x, y, ...) v_event_time(x)
vec_cast.numeric.v_event_time <- function(x, y, ...) vctrs::vec_data(x)

#' @method vec_cast.v_event_time numeric
#' @export
vec_cast.v_event_time.numeric <- function(x, y, ...) v_event_time(x)
vec_cast.numeric.v_event_time <- function(x, y, ...) vctrs::vec_data(x)

#' Casting function for count objects
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

# Restoration ####
#' @importFrom vctrs vec_restore
#' @method vec_restore v_event_time
#' @export
vec_restore.v_event_time <- function(x, to, ..., i = NULL) {
  # Update description
  desc    <- describe(vctrs::vec_data(x))
  # Maintain context
  context <- get_context(to)
  
  new_event_time(x, .desc = desc, .context = context)
}

# Math Operations ####

# Print foot
#' @importFrom vctrs obj_print_footer
#' @method obj_print_footer v_event_time
#' @export
obj_print_footer.v_event_time <- function(x, ...) {
  cat("# Median survival: ", attr(x, "desc")[["median"]], "\n", sep = "")
}

#' @importFrom vctrs vec_ptype_full
#' @method vec_ptype_full v_event_time
#' @export
vec_ptype_full.v_event_time <- function(x) {
  "event_time"
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