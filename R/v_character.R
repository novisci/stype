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
                          .context = context(),
                          .extra_descriptors = list()){
  # x <- vctrs::vec_cast(x, character())
  # vctrs::vec_assert(x, ptype = character())
  vctrs::new_vctr(
    x, 
    internal_name = .internal_name,
    data_summary  = .data_summary, 
    context       = .context, 
    extra_descriptors = .extra_descriptors,
    class = "v_character")
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_character", "vctrs_vctr"))

#' Character constructor
#' @param x a \code{character} vector
#' @rdname v_character 
#' @export
v_character <- make_stype_constructor(
  typeFUN = new_character,
  ptypeFUN = character,
  dataFUN = vctrs::vec_data
)

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

#' @method vec_ptype2.v_character v_character
#' @export
vec_ptype2.v_character.v_character <- function(x, y, ...){
  compare_contexts(x, y)
  check_internal_names(x, y)
  
  v_character(
    internal_name = get_internal_name(x),
    context = get_context(x))
}

#' @method vec_ptype2.v_character character
#' @export
vec_ptype2.v_character.character <- function(x, y, ...) { x } 

#' @rdname casting
#' @method vec_cast v_character
#' @export
#' @export vec_cast.v_character
vec_cast.v_character <- function(x, to, ...) UseMethod("vec_cast.v_character")

#' @method vec_cast.v_character v_character
#' @export
vec_cast.v_character.v_character <- function(x, to, ...) {
  v_character(vctrs::vec_data(x), 
              internal_name = get_internal_name(to),
              context = get_context(to))
}

#' Casting function for character objects
#' @rdname v_character 
#' @export
as_character <- function(x) {
  vctrs::vec_cast(x, new_character())
}

#' @rdname v_character 
#' @export
as.character.v_character <- function(x, ...) {
  as.character(as_canonical(x))
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