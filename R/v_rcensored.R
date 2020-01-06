#' Right censored vectors
#' 
#' TODO: add a description
#' 
#' @name v_rcensored
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_ptype2 vec_data new_vctr vec_assert vec_arith_base
#' @inheritParams v_count
#' @family stype types
NULL

#' The internal builder of v_rcensored
#' @noRd
#' @param .internal_name the internal name of the variable
#' @param .data_summary a \code{\link{data_summary}}
#' @param .context a \code{\link{context}}
#' @importFrom vctrs new_rcrd
#' @keywords internal

new_rcensored <- function(time           = v_event_time(),
                          censored       = v_binary(),
                          outcome        = v_binary(),
                          censor_reason  = v_nominal(),
                          outcome_reason = v_nominal(),
                          .end_time      = numeric(),
                          .internal_name = character(), 
                          .data_summary  = data_summary(), 
                          .context       = context()){
  
  vctrs::vec_assert(time, ptype = v_event_time())
  vctrs::vec_assert(censored, ptype = v_binary())
  vctrs::vec_assert(outcome, ptype = v_binary())
  
  # TODO: 
  # Fix following error when following vec_assert are uncommented: 
  #  `outcome_reason` must be a vector with type <nominal>.
  #  Instead, it has type <nominal>.
  # vctrs::vec_assert(outcome_reason, ptype = v_nominal())
  # vctrs::vec_assert(censor_reason, ptype = v_nominal())
  
  vdat <- list(
    time = time,
    censored = censored,
    outcome  = outcome,
    censor_reason  = censor_reason,
    outcome_reason = outcome_reason
  )
  
  vctrs::new_rcrd(
    vdat, 
    internal_name = .internal_name,
    data_summary  = .data_summary, 
    context       = .context, 
    end_time      = .end_time,
    class         = c("v_rcensored", "censored"))
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_rcensored", "v_censored", "vctrs_vctr"))

#' Right-censored constructor
#' 
#' @param outcomes A \code{list} of \code{v_event_time} vectors that define the 
#' outcomes. The order of this list defines the precedence of outcomes. That is,
#' if the first outcome and second outcome occur at the same time, the first 
#' outcome is the reason for the outcome.
#' @param censors A \code{list} of \code{v_event_time} vectors that define the 
#' censor The order of this list defines the precedence of censoring That is,
#' if the first censor and second censor occur at the same time, the first 
#' censor is the reason for the censoring
#' @param end_time A \code{numeric} scalar defining the end of follow-up.
#' @rdname v_rcensored 
#' @export

v_rcensored <- function(outcomes = list(), 
                        censors = list(),
                        end_time = Inf,
                        internal_name = "", 
                        context){
  
  if(is_event_time(outcomes)){
    outcomes <- list(outcomes)
  }
  
  if(is_event_time(censors)){
    censors <- list(censors)
  }
  
  # TODO: drop requirement that inputs be v_event_time()? 
  purrr::walk(
    .x = append(outcomes, censors),
    .f = ~ is_event_time(.x)
  )
  
  vctrs::vec_assert(end_time, ptype = numeric(), size = c(1L))
  
  vdat <- .v_rcensored(outcomes, censors, end_time)
  
  if(missing(context)){
    context <- methods::new("context")
  }
  
  out <- new_rcensored(
    time           = vdat[["times"]], 
    censored       = vdat[["censored"]],
    outcome        = vdat[["outcome"]],
    outcome_reason = vdat[["outcome_reason"]],
    censor_reason  = vdat[["censor_reason"]], 
    .end_time      = end_time,
    .internal_name = internal_name,
    .data_summary  = data_summary(),
    .context       = context)
  
  dsum <- describe(out)
  attr(out, "data_summary") <- dsum
  out
}


#' Right-censored constructor
#' @param ... TODO
#' @noRd
gather_time_reason <- function(...){
  
  dots <- c(...)
  hold <- which.min(dots)
  
  list(
    time   = purrr::lift_dl(pmin)(dots, na.rm = TRUE),
    reason = if (length(hold) == 0) 0 else hold
  )
}

#' Right-censored constructor
#' @param ... TODO
#' @noRd
gather_times_reasons <- function(times, levs){
  
  itimes <- purrr::map(times, as_canonical)
  
  hold <- purrr::pmap(
    .l = itimes, 
    .f = gather_time_reason
  )
  
  list(
    times   = v_event_time(purrr::map_dbl(hold, "time")),
    reasons = v_nominal(factor(levs[purrr::map_dbl(hold, "reason") + 1], 
                               levels = levs))
  )
}

#' Internal function for getting levels and labels for censoring and outcomes
#' @noRd

get_levels_labels <- function(x){
  
  # - handle the case that neither internal name *nor* short labels are defined
  # - handle the case that either  internal name *or* short labels are defined
  # - handle the case that both internal name and short labels are defined
  poss_levels <- purrr::map_chr(x, ~ attr(.x, "internal_name"))
  poss_labels <- purrr::map_chr(x, ~ get_context(.x)@short_label)
  poss_pos    <- 1:length(x)
  
  has_empty_levels <- any(poss_levels == "")
  has_empty_labels <- any(poss_labels == "")
  has_all_levels   <- all(poss_levels != "")
  has_all_labels   <- all(poss_labels != "")
  
  if ( has_empty_levels && has_empty_labels ) {
    poss_levels <- poss_labels <- poss_pos
  } else if ( has_all_levels && has_empty_labels ){
    poss_labels <- poss_levels
  } else if ( has_empty_levels && has_all_labels ){
    poss_levels <- poss_labels
  }

  list(levels = poss_levels, labels = poss_labels)
}


#' Internal Right-censored constructor
#' @noRd
.v_rcensored <- function(outcomes, censors, end_time){
  
  clev <- get_levels_labels(censors)
  olev <- get_levels_labels(outcomes)

  cens <- gather_times_reasons(censors, c("not_censored", clev[["levels"]]))
  outs <- gather_times_reasons(outcomes,c("not_outcome", olev[["levels"]]))
  
  hold <- gather_times_reasons(
    append(list(outs[["times"]], cens[["times"]]), list(v_event_time(end_time))),
    levs =  c("none", "outcome", "censor", "admin")
  )
  
  creas <- cens[["reasons"]]
  creas[hold[["reasons"]] != "censor"] <- NA
  creas <- factor(as.character(creas), 
                  levels = clev[["levels"]],
                  labels = clev[["labels"]])
  
  oreas <- outs[["reasons"]]
  oreas[hold[["reasons"]] != "outcome"] <- NA
  oreas <- factor(as.character(oreas), 
                  levels = olev[["levels"]],
                  labels = olev[["labels"]])
  
  list(
    times    = hold[["times"]],
    censored = v_binary(hold[["reasons"]] == "censor"),
    outcome  = v_binary(hold[["reasons"]] == "outcome"),
    censor_reason  = v_nominal(creas),
    outcome_reason = v_nominal(oreas)
  )
}


# Formatting ####
#' @method format v_rcensored
#' @export
format.v_rcensored <- function(x, ...) {
  tm <- vctrs::vec_data(vctrs::field(x, "time"))
  d1 <- vctrs::vec_data(vctrs::field(x, "censored"))
  d2 <- vctrs::vec_data(vctrs::field(x, "outcome"))
  
  symbol <- ifelse(
    !d1 & !d2, "\U25B8",
    ifelse(d1, "\U25B9", " ")
  )
  
  sprintf("%s%s", tm, symbol)
}

#' Predicate function for count objects
#' @rdname v_rcensored
#' @export

is_rcensored <- function(x){
  inherits(x, "v_rcensored")
}

# Casting and coercing ####

#' Casting
#' @name casting
#' @inheritParams vctrs::vec_ptype2
#' @method vec_ptype2 v_rcensored
#' @export
#' @export vec_ptype2.v_rcensored
vec_ptype2.v_rcensored <- function(x, y, ...) UseMethod("vec_ptype2.v_rcensored", y)

#' @method vec_ptype2.v_rcensored default
#' @export
vec_ptype2.v_rcensored.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.v_rcensored vctrs_unspecified
#' @export
vec_ptype2.v_rcensored.vctrs_unspecified <- function(x, y, ...) x

#' @method vec_ptype2.v_rcensored v_rcensored
#' @export
vec_ptype2.v_rcensored.v_rcensored <- function(x, y, ...) new_rcensored()

#' @rdname casting
#' @inheritParams vctrs::vec_cast
#' @method vec_cast v_rcensored
#' @export
#' @export vec_cast.v_rcensored
vec_cast.v_rcensored <- function(x, to, ...) UseMethod("vec_cast.v_rcensored")

#' @method vec_cast.v_rcensored v_rcensored
#' @export
vec_cast.v_rcensored.v_rcensored <- function(x, to, ...) x

#' @method vec_cast.v_rcensored default
#' @export
vec_cast.v_rcensored.default  <- function(x, to, ...) vctrs::vec_default_cast(x, to)


#' @rdname v_rcensored 
#' @export
# TODO: Is the canonical acutally a Surv object
as_canonical.v_rcensored <- function(x){
  as.list(vctrs::vec_data(x))
}

#' @importFrom vctrs obj_print_footer
#' @importFrom crayon "%+%"
#' @method obj_print_footer v_rcensored
#' @export
obj_print_footer.v_rcensored <- function(x, ...) {
  
  # cent <- get_data_summary(vctrs::field(x, "censor_reason"), "ptable")
  out <- desc_printer(vctrs::field(x, "censored"), "Proportion censored", "proportion")
  cat(out, "\n")
}


#' @importFrom vctrs vec_ptype_full
#' @method vec_ptype_full v_rcensored
#' @export
vec_ptype_full.v_rcensored <- function(x) {
  "right censored"
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr v_rcensored
#' @export
vec_ptype_abbr.v_rcensored <- function(x) {
  "rcen"
}

#' @importFrom pillar type_sum
#' @method type_sum v_rcensored
#' @export
type_sum.v_rcensored <- function(x) {
  "rcen"
}
