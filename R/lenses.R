#' Lenses for stype vectors
#' 
#' @name stype_lenses
#' @param predicate a predicate function to apply to each element of the list 
#'     (or each column of a \code{data.frame})
#' @importFrom lenses lens attr_l set view "%.%"
#' @importFrom purrr map_lgl 
# @export
NULL

#' @describeIn stype_lenses Creates a lens for stype variables with a list-like 
#'       structure
#' @export
stype_df_l <- function(predicate){
  lenses::lens(
    view = function(d) {
      where_to_pull <- purrr::map_lgl(d, predicate)
      d[where_to_pull]
    },
    set  = function(d, x) {
      where_to_replace <- purrr::map_lgl(d, predicate)
      
      assertthat::assert_that(
        sum(where_to_replace) == length(x),
        msg = sprintf(
          "# of elements satisfied by the predicate (%s) != # of replacement elements (%s)",
           sum(where_to_replace), length(x))
      )
    
      d[where_to_replace] <- x 
      d
    }
  )
}

#' @describeIn stype_lenses view/set \code{\link{v_binary}} stype vectors
#' @export
binary_l <- stype_df_l(is_binary)

#' @describeIn stype_lenses view/set \code{\link{v_count}} stype vectors
#' @export
count_l <- stype_df_l(is_count)

#' @describeIn stype_lenses view/set \code{\link{v_rcensored}} stype vectors
#' @export
rcensored_l <- stype_df_l(is_rcensored)

#' @describeIn stype_lenses view/set \code{outcome} stype vectors
#' @export
outcome_l <- stype_df_l(is_outcome)

#' @describeIn stype_lenses view/set \code{covariate} stype vectors
#' @export
covariate_l <- stype_df_l(is_covariate)

#' @describeIn stype_lenses view/set \code{weight} stype vectors
#' @export
weight_l <- stype_df_l(is_weight)

#' @describeIn stype_lenses view/set constant stype vectors
#' @export
constant_l <- stype_df_l(is_constant)

#' @describeIn stype_lenses view/set not constant stype vectors
#' @export
not_constant_l <- stype_df_l(is_not_constant)

#' @describeIn stype_lenses view/set the \code{data_summary} object of a stype vector
#' @export
data_summmary_l <- lenses::attr_l("data_summary")

#' @describeIn stype_lenses view/set the \code{\link[vctrs]{vec_data}} of a
#'      \code{stype}. Casts to the original type when \code{set}.
#' @export
vec_data_l <- lenses::lens(
  view = function(d) vctrs::vec_data(d),
  set  = function(d, x) vctrs::vec_c(vctrs::vec_ptype(d), x)
)

#' @describeIn stype_lenses view/set the \code{\link{context}} of a \code{stype}. 
#' @export
context_l <- lenses::attr_l("context")

#' @describeIn stype_lenses view/set the \code{internal_name} of a \code{stype}. 
#' @export
internal_name_l <- lenses::attr_l("internal_name")