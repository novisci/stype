#------------------------------------------------------------------------------#
# Package description plus:
# - define collation order of .R files (last to first)

#------------------------------------------------------------------------------#
#' Statistical algebraic data types
#'
#' \code{stype} (pronounced "stipe") uses the \link[vctrs]{vctrs} package
#' to implement algebraic data types that:
#' 
#' \itemize{
#'  \item provide S3 classes aligned with statistical language: binary, 
#'  continuous, count, non-negative continuous, time to event, etc;
#'  \item create variable-level metadata useful for sharing information 
#'  about a variable across applications;
#'  \item add automatically generated summary statistics to the variable metadata.
#' }
#' 
#' @include lenses.R
#' @include predicates.R
#' @include context.R
#' @include describe.R
#' @include data_summary.R
#' @include purpose.R
#' @include tbl_analysis.R 
#' @include v_rcensored.R
#' @include v_character.R
#' @include v_ordered.R
#' @include v_nominal.R
#' @include v_binary.R
#' @include v_event_time.R
#' @include v_continuous_nonneg.R
#' @include v_continuous.R
#' @include v_count.R
#' @include utilities.R
"_PACKAGE"

#' Display an object
#' @name show-methods
#' @importFrom methods show
#' @param object the object to display
NULL