#' Character vectors
#'
#' @description {
#' Support: see \code{\link{character}}
#' 
#' Prototype: \code{\link{character}}
#' } 
#' 
#' @name v_character
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_ptype2 vec_data new_vctr vec_assert vec_arith_base
#' @inheritParams v_count
#' @family stype types

NULL

#' The internal builder of v_character 
#' @noRd
#' @keywords internal
new_character <- function(x = character(), 
                          .internal_name = character(),
                          .data_summary = data_summary(), 
                          .context = context()){
  # x <- vctrs::vec_cast(x, character())
  # vctrs::vec_assert(x, ptype = character())
  vctrs::new_vctr(
    x, 
    internal_name = .internal_name,
    data_summary  = .data_summary, 
    context       = .context, 
    class = "v_character")
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_character", "vctrs_vctr"))

#' Character constructor
#' @param x a \code{character} vector
#' @rdname v_character 
#' @export

v_character <- function(x = character(), internal_name = "", context){
  # x <- vctrs::vec_cast(x, character())
  dsum <- describe(vctrs::vec_data(x))
  if(missing(context)){
    context <- methods::new("context")
  }
  
  new_character(
    x, 
    .internal_name = internal_name,
    .data_summary  = dsum,
    .context       = context)
}

#' Predicate function for count objects
#' @rdname v_character 
#' @export

is_character <- function(x){
  inherits(x, "v_character")
}

# Casting and coercing ####
#' @rdname casting
#' @method vec_ptype2 v_character
#' @export
#' @export vec_ptype2.v_character
vec_ptype2.v_character <- function(x, y, ...) UseMethod("vec_ptype2.v_character", y)

#' @method vec_ptype2.v_character default
#' @export
vec_ptype2.v_character.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.v_character vctrs_unspecified
#' @export
vec_ptype2.v_character.vctrs_unspecified <- function(x, y, ...) x

#' @method vec_ptype2.v_character v_character
#' @export
vec_ptype2.v_character.v_character <- function(x, y, ...){
  compare_contexts(x, y)
  v_character(context = get_context(x))
}

#' @method vec_ptype2.v_character character
#' @export
vec_ptype2.v_character.character <- function(x, y, ...) { x } 

#' @method vec_ptype2.character v_character
#' @importFrom vctrs vec_ptype2.character
#' @export 
vec_ptype2.character.v_character <- function(x, y, ...) { x }

#' @rdname casting
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

#' @method vec_cast.v_character character
#' @export
vec_cast.v_character.character <- function(x, to, ...) { 
  v_character(vctrs::vec_data(x)) 
}

#' @method vec_cast.character v_character
#' @importFrom vctrs vec_cast.character 
#' @export
vec_cast.character.v_character<- function(x, to, ...) { vctrs::vec_data(x) }


#' Casting function for character objects
#' @rdname v_character 
#' @export
as_character <- function(x) {
  vctrs::vec_cast(x, new_character())
}

#' @rdname v_character
#' @export
as_canonical.v_character <- function(x){
  as.character(vctrs::vec_data(x))
}

# Restoration ####
#' @importFrom vctrs vec_restore
#' @method vec_restore v_character
#' @export
vec_restore.v_character <- function(x, to, ..., i = NULL) {
  iname   <- attr(to, "internal_name")
  # Update description
  desc    <- describe(vctrs::vec_data(x))
  # Maintain context
  context <- get_context(to)
  
  new_character(
    x,
    .internal_name = iname,
    .data_summary  = desc, 
    .context       = context)
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
  print_footer(
    x, 
    c(max_char = "Max char", min_char = "Min char", n_unique = "# unique"))

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