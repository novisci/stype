#' Right censored vectors
#' 
#'  Some desc
#' 
#' @name v_rcensored
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_ptype2 vec_data new_vctr vec_assert vec_arith_base
#' @param x a \code{integer} vector
#' @param internal_name the internal name of the variable
#' @param context a \code{\link{context}}
NULL

#' The internal builder of v_rcensored
#' @noRd
#' @param .internal_name the internal name of the variable
#' @param .data_summary a \code{\link{data_summary}}
#' @param .context a \code{\link{context}}
#' @importFrom vctrs new_rcrd
#' @keywords internal

new_rcensored <- function(time     = v_event_time(),
                          censored = v_binary(),
                          outcome  = v_binary(),
                          censor_reason = v_nominal(),
                          outcome_reason = v_nominal(),
                          .internal_name = character(), 
                          .data_summary = data_summary(), 
                          .context = context()){
  
  # vctrs::vec_assert(x, ptype = integer())
  # vctrs::vec_assert(desc, ptype = description())
  
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
    class         = c("v_rcensored", "censored"))
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_rcensored", "v_censored", "vctrs_vctr"))

#' Right-censored constructor
#' 
#' constructor function for right censored objects
#' 
#' @param outcomes a \code{list} of \code{v_event_time} vectors
#' @param censors a \code{list} of \code{v_event_time} vectors
#' @param end_time a scalar
#' @rdname v_rcensored 
#' @export

v_rcensored <- function(outcomes = list(), 
                        censors = list(),
                        end_time = Inf,
                        internal_name = "", context){
  
  # TODO: assert that inputs are v_event_times
  # TODO: assert that end_time is length one
  
  vdat <- .v_rcensored(outcomes, censors, end_time)
  
  if(missing(context)){
    context <- methods::new("context")
  }
  
  out <- new_rcensored(
    time           = vdat$time, 
    censored       = vdat$censored,
    outcome        = vdat$outcome,
    outcome_reason = vdat$outcome_reason,
    censor_reason  = vdat$censor_reason, 
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

#' Internal Right-censored constructor
#' @noRd
.v_rcensored <- function(outcomes, censors, end_time){
  
  #TODO: 
  # - handle the case that neither internal name *nor* short labels are defined
  # - handle the case that either  internal name *or* short labels are defined
  # - handle the case that both internal name and short labels are defined
  poss_creasons_levels <- purrr::map_chr(censors, ~ attr(.x, "internal_name"))
  poss_creasons_labels <- purrr::map_chr(censors, ~ get_context(.x)@short_label)
  
  if(any(poss_creasons_labels == "")){
    poss_creasons_labels <- poss_creasons_levels
  }
  
  poss_oreasons_levels <- purrr::map_chr(outcomes, ~ attr(.x, "internal_name"))
  poss_oreasons_labels <- purrr::map_chr(outcomes, ~ get_context(.x)@short_label)
  
  if(any(poss_oreasons_labels == "")){
    poss_oreasons_labels <- poss_oreasons_levels
  }
  
  cens <- gather_times_reasons(
    censors,
    c("not_censored", poss_creasons_levels)
  )
  
  outs <- gather_times_reasons(
    outcomes,
    c("not_outcome", poss_oreasons_levels)
  )
  
  hold <- gather_times_reasons(
    append(list(outs$times, cens$times), list(v_event_time(end_time))),
    levs =  c("none", "outcome", "censor", "admin")
  )
  
  creas <- cens$reasons
  creas[hold$reasons != "censor"] <- NA
  creas <- factor(as.character(creas), 
                  levels = poss_creasons_levels,
                  labels = poss_creasons_labels)
  
  oreas <- outs$reasons
  oreas[hold$reasons != "outcome"] <- NA
  oreas <- factor(as.character(oreas), 
                  levels = poss_oreasons_levels,
                  labels = poss_oreasons_labels)
  
  list(
    times    = hold$times,
    censored = v_binary(hold$reasons == "censor"),
    outcome  = v_binary(hold$reasons == "outcome"),
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
