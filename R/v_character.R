#' Character vectors
#' 
#'  Some desc
#' 
#' @name v_character
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_type2 vec_data new_vctr vec_assert vec_arith_base
#' @inheritParams v_count

new_character <- function(x = character(), .desc = description(), .context = context()){
  x <- vctrs::vec_cast(x, character())
  vctrs::vec_assert(x, ptype = character())
  vctrs::new_vctr(x, desc = .desc, context = .context, class = "v_character")
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_character", "vctrs_vctr"))

#' Character constructor
#' 
#' constructor function for character objects
#' @param x a \code{character} vector
#' @rdname v_character 
#' @export

v_character <- function(x = character(), context){
  # x <- vctrs::vec_cast(x, character())
  desc <- describe(vctrs::vec_data(x))
  if(missing(context)){
    context <- methods::new("context")
  }
  new_character(x, .desc = desc, .context = context)
}

#' Predicate function for count objects
#' @rdname v_character 
#' @export

is_character <- function(x){
  inherits(x, "v_character")
}

# Casting and coercing ####

#' @method vec_type2 v_character
#' @export
#' @export vec_type2.v_character
vec_type2.v_character <- function(x, y, ...) UseMethod("vec_type2.v_character", y)

#' @method vec_type2.v_character default
#' @export
vec_type2.v_character.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_type2.v_character vctrs_unspecified
#' @export
vec_type2.v_character.vctrs_unspecified <- function(x, y, ...) x

#' @method vec_type2.v_character v_character
#' @export
vec_type2.v_character.v_character <- function(x, y, ...){
  compare_contexts(x, y)
  v_character(context = get_context(x))
}

#' @method vec_cast v_character
#' @export
#' @export vec_cast.v_character
vec_cast.v_character <- function(x, to, ...) UseMethod("vec_cast.v_character")

#' @method vec_cast.v_character v_character
#' @export
vec_cast.v_character.v_character <- function(x, to, ...) {
  v_character(vctrs::vec_data(x), context = get_context(to))
}

#' @method vec_cast.v_character default
#' @export
vec_cast.v_character.default  <- function(x, to, ...) vctrs::vec_default_cast(x, to)

#' Casting function for count objects
#' @rdname v_character 
#' @export
as_character <- function(x) {
  vctrs::vec_cast(x, new_character())
}

# Restoration ####
#' @importFrom vctrs vec_restore
#' @method vec_restore v_character
#' @export
vec_restore.v_character <- function(x, to, ..., i = NULL) {
  # Update description
  desc    <- describe(vctrs::vec_data(x))
  # Maintain context
  context <- get_context(to)
  
  new_character(x, .desc = desc, .context = context)
}

# Formatting ####
#' @method format v_character
#' @export
format.v_character <- function(x, ...) {
  #TODO
  vctrs::vec_data(x)
}

# Print foot
#' @importFrom vctrs obj_print_footer
#' @method obj_print_footer v_character
#' @export
obj_print_footer.v_character <- function(x, ...) {
  has_miss <- attr(x, "desc")[["has_missing"]]
  has_ctxt <- !is_empty(get_context(x))
  cat("# TODO") 
  # cat("# Proportion: ", round(attr(x, "desc")[["proportion"]], 2), 
  #     if(has_miss){
  #       paste0("; Missing: ", attr(x, "desc")[["n_missing"]])
  #     } else {
  #       ""
  #     },
  #     "\n", 
  #     if(has_ctxt){
  #       paste0("# Purpose: ", 
  #              methods::slot(get_context(x), "purpose"),
  #              "\n")
  #     } else {
  #       ""
  #     },
  #     sep = "")
}

#' @importFrom vctrs vec_ptype_full
#' @method vec_ptype_full v_character
#' @export
vec_ptype_full.v_character <- function(x) {
  "character"
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr v_character
#' @export
vec_ptype_abbr.v_character <- function(x) {
  "chr"
}

#' @importFrom pillar type_sum
#' @export
type_sum.v_character <- function(x) {
  "chr"
}