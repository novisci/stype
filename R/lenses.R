#' Lenses for stype vectors
#' 
#' @name stype_lenses
#' @param predicate a predicate function to apply to each element of the list 
#'     (or each column of a \code{data.frame})
#' @param tags a \code{character} vector of tags
#' @importFrom lenses lens attr_l set view "%.%" slot_l over over_map over_with
#' @importFrom purrr map_lgl 
# @export
NULL

#' @export
lenses::view

#' @export
lenses::set

#' @export
lenses::over_map

#' @export
lenses::over

#' @export
lenses::over_map

#' @export
lenses::`%.%`

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

# Type-based lenses ####
#' @describeIn stype_lenses view/set \code{\link{v_binary}} stype vectors
#' @export
binary_l <- stype_df_l(is_binary)

#' @describeIn stype_lenses view/set \code{\link{v_character}} stype vectors
#' @export
character_l <- stype_df_l(is_character)

#' @describeIn stype_lenses view/set \code{\link{v_continuous}} stype vectors
#' @export
continuous_l <- stype_df_l(is_continuous)

#' @describeIn stype_lenses view/set \code{\link{v_continuous_nonneg}} stype vectors
#' @export
continuous_nonneg_l <- stype_df_l(is_continuous_nonneg)

#' @describeIn stype_lenses view/set \code{\link{v_count}} stype vectors
#' @export
count_l <- stype_df_l(is_count)

#' @describeIn stype_lenses view/set \code{\link{v_event_time}} stype vectors
#' @export
event_time_l <- stype_df_l(is_event_time)

#' @describeIn stype_lenses view/set \code{\link{v_nominal}} stype vectors
#' @export
nominal_l <- stype_df_l(is_nominal)

#' @describeIn stype_lenses view/set \code{\link{v_ordered}} stype vectors
#' @export
ordered_l <- stype_df_l(is_ordered)

#' @describeIn stype_lenses view/set \code{\link{v_rcensored}} stype vectors
#' @export
rcensored_l <- stype_df_l(is_rcensored)

# Role-based lenses ####

#' @describeIn stype_lenses view/set stype vectors with \code{covariate} role
#' @export
covariate_l <- stype_df_l(is_covariate)

#' @describeIn stype_lenses view/set stype vectors with \code{censoring} role
#' @export
censoring_l <- stype_df_l(is_censoring)

#' @describeIn stype_lenses view/set stype vectors with \code{competing} role
#' @export
competing_l <- stype_df_l(is_competing)
#' @describeIn stype_lenses view/set stype vectors with \code{exposure} role
#' @export
exposure_l <- stype_df_l(is_exposure)

#' @describeIn stype_lenses view/set stype vectors with \code{identifier} role
#' @export
identifier_l <- stype_df_l(is_identifier)

#' @describeIn stype_lenses view/set stype vectors with \code{index} role
#' @export
index_l <- stype_df_l(is_index)

#' @describeIn stype_lenses view/set stype vectors with \code{is_intermediate} role
#' @export
intermediate_l <- stype_df_l(is_intermediate)

#' @describeIn stype_lenses view/set stype vectors with \code{other} role
#' @export
other_l <- stype_df_l(is_other)

#' @describeIn stype_lenses view/set stype vectors with \code{outcome} role
#' @export
outcome_l <- stype_df_l(is_outcome)

#' @describeIn stype_lenses view/set stype vectors with  \code{weight} role
#' @export
weight_l <- stype_df_l(is_weight)


# Lenses based on other predicates #### 
#' @describeIn stype_lenses view/set constant stype vectors
#' @export
constant_l <- stype_df_l(is_constant)

#' @describeIn stype_lenses view/set not constant stype vectors
#' @export
not_constant_l <- stype_df_l(is_not_constant)

#' @describeIn stype_lenses view/set stype vectors tagged with \code{tags}
#' @export
tag_l <- function(tags){
  .predicate <- function(x) is_tagged(x, tags = tags)
  stype_df_l(.predicate)
}

# Lenses for data within a stype ####
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

#' @describeIn stype_lenses view/set the \code{extra_descriptors} of a \code{stype}. 
#' @export
extra_descriptors_l <- lenses::attr_l("extra_descriptors")

#' @describeIn stype_lenses view/set the \code{derivation} slot of a \code{stype}
#'    \code{\link{context}}. 
#' @export
derivation_l <- context_l %.% slot_l("derivation")

#' @describeIn stype_lenses view/set the \code{short_label} slot of a \code{stype}
#'    \code{\link{context}}. 
#' @export
short_label_l <- context_l %.% slot_l("short_label") 

#' @describeIn stype_lenses view/set the \code{long_label} slot of a \code{stype}
#'    \code{\link{context}}. 
#' @export
long_label_l <- context_l %.% slot_l("long_label")  

#' @describeIn stype_lenses view/set the \code{description} slot of a \code{stype}
#'    \code{\link{context}}. 
#' @export
description_l <- context_l %.% slot_l("description")  

#' @describeIn stype_lenses view/set the \code{security_type} slot of a \code{stype}
#'    \code{\link{context}}. 
#' @export
security_type_l <- context_l %.% slot_l("security_type") 

#' @describeIn stype_lenses view/set the \code{purpose} slot of a \code{stype}
#'    \code{\link{context}}. 
#' @export
purpose_l <- context_l %.% slot_l("purpose")

#' @describeIn stype_lenses view/set the \code{study_role} slot of a \code{stype}
#'    \code{\link{context}}'s \code{\link{purpose}}. 
#' @export
study_role_l <- context_l %.% slot_l("purpose") %.% slot_l("study_role") 

#' @describeIn stype_lenses view/set the \code{tags} slot of a \code{stype}
#'    \code{\link{context}}'s \code{\link{purpose}}. 
#' @export
tags_l <- context_l %.% slot_l("purpose") %.% slot_l("tags")  