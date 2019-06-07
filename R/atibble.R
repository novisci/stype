#------------------------------------------------------------------------------#
# Defines the following
#
# - atibble: a tibble composed entirely of variables

# tbl_df
# @name tbl_df-class
# @aliases tbl_df
# @exportClass tbl_df

#' @export
#' @importClassesFrom tibble tbl_df
NULL
# setOldClass(c("tbl_df", "tbl", "data.frame"), "tbl_df")


#' atibble = analysis tibble
#' 
#' @slot name a name
#' @slot label a label
#' @export atibble
atibble <- setClass(
  "atibble",
  slots = c(
    "name"      = "character",
    "label"     = "character"
    # TODO: what other slots would be useful?
    # "timestamp" = "character" 
  ),
  contains = "tbl_df"
)

setMethod(
  f          = "initialize",
  signature  = "atibble",
  definition = function(.Object, name, label, ...){
    
    vars <- list(...)
    # vars <- purrr::set_names(vars, purrr::map(vars, ~ .x@name))

    # browser()
    dt <- tibble::tibble(
      !!! vars,
      .name_repair = "check_unique"
    )
 
    callNextMethod(.Object, name = name, label = label, dt)
  }
)
setValidity(
  Class  = "atibble",
  method = function(object){
    # assertthat::validate_that(
    #   all(purrr::map_lgl(object, ~ methods::is(.x, "variable"))),
    #   msg = "All columns of a tibble must be S4 variable class objects."
    # )
    
    TRUE
  }
)

#' @rdname glance
#' @importFrom purrr map_dfr
#' @export

setMethod(
  f          = "glance",
  signature  = "atibble",
  definition = function(object){
    purrr::map_dfr(object, ~ describe(.x))
  }
)
