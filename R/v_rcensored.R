#' Right censored vectors
#' 
#' `v_rcensored` provides a right-censored vector. Unlike other representations, 
#' such the `survival` package's `Surv` object, `v_rcensored` can be subset with 
#' `[` and concatenated with `c` as you would any other vector. The type is 
#' implemented as a \code{\link[vctrs]{new_rcrd}} where the necessary data are
#' contained in \code{\link[vctrs]{fields}}.
#' 
#' `as_canonical` casts the vector to a `list`. See \code{\link{v_rcensored_accessors}} 
#' for functions to access components of a `v_rcensored`.
#' 
#' When printed, an open right triangle indicates an observation was censored. 
#' A closed right triangle indicates an observation reached `end_time` without 
#' being censored or having an outcome. No triangles indicated an observation
#' that had one of the outcomes.
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
#' @param .extra_descriptors A \code{list} of \code{\link{descriptors}} functions
#'        appended to the default \code{\link{descriptors}}.
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
                          .context       = context(),
                          .extra_descriptors = list()){
 
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
    extra_descriptors = .extra_descriptors,
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
#' @param extra_descriptors A \code{list} of \code{\link{descriptors}} functions
#'        appended to the default \code{\link{descriptors}}.
#' @importFrom vctrs vec_recycle vec_size
#' @rdname v_rcensored 
#' @export
v_rcensored <- function(outcomes = list(), 
                        censors,
                        end_time = Inf,
                        internal_name = "", 
                        context,
                        extra_descriptors = list()){
  
  if(is_event_time(outcomes)){
    outcomes <- list(outcomes)
  }
  
  if(missing(censors)){
    censors <- vctrs::vec_recycle(
      v_event_time(NA_real_),
      if (vctrs::vec_size(outcomes) == 0L) 0L
      else vctrs::vec_size(outcomes[[1]]))
  }
  
  if(is_event_time(censors)){
    censors <- list(censors)
  }
  
  # TODO: drop requirement that inputs be v_event_time()?
  # FIXME: this doesn't actually do anything, right?
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
    .internal_name = check_internal_name(internal_name),
    .data_summary  = data_summary(),
    .context       = context,
    .extra_descriptors = extra_descriptors)
  
  dsum <- describe(out, .descriptors = extra_descriptors)
  attr(out, "data_summary") <- dsum
  out
}

#' @rdname v_rcensored 
#' @export
rcen <- v_rcensored

#' Predicate function for right-censored objects
#' @rdname v_rcensored
#' @export
is_v_rcensored <- function(x){
  inherits(x, "v_rcensored")
}

#' Internal function for finding the times/reasons for a right censored stype
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
    # reasons = factor(levs[purrr::map_dbl(hold, "reason") + 1], levels = levs)
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
    append(list(outs[["times"]], cens[["times"]]), 
           list(v_event_time(end_time))),
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

#' The invers-ish of .v_rcensored
#' @importFrom vctrs field
#' @importFrom purrr set_names
#' @noRd
.v_rcensored_unpack <- function(x){
  
  time  <- vctrs::field(x, "time")
  
  oreas <- vctrs::field(x, "outcome_reason")
  ocomes <- purrr::map(
    .x = purrr::set_names(levels(oreas)),
    .f = ~ {
      hold <- time
      idx  <- oreas == .x
      hold[!ifelse(is.na(idx), FALSE, idx)] <- NA_real_
      attr(hold, "internal_name") <- .x
      hold
    }
  )
  
  creas <- vctrs::field(x, "censor_reason")
  censr <-  purrr::map(
    .x = purrr::set_names(levels(creas)),
    .f = ~ {
      hold <- time
      idx  <- creas == .x
      hold[!ifelse(is.na(idx), FALSE, idx)] <- NA_real_
      attr(hold, "internal_name") <- .x
      hold
    }
  )
  
  etime <- attr(x, "end_time")
  list(outcomes = ocomes, censors = censr, end_time = etime)
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
vec_ptype2.v_rcensored <- function(x, y, ...) {
  UseMethod("vec_ptype2.v_rcensored", y)
}

#' @method vec_ptype2.v_rcensored v_rcensored
#' @export
vec_ptype2.v_rcensored.v_rcensored <- function(x, y, ...) {
  
  compare_contexts(x, y)
  check_internal_names(x, y)
  
  assertthat::assert_that(
    attr(x, "end_time") == attr(y, "end_time") || 
      # Handle case where either end_time is missing
      # (which can happen on vec_restore)
      length(attr(x, "end_time")) == 0 || 
      length(attr(y, "end_time")) == 0,
    msg = "x and y must have the same end time."
  )
 
  oreasx <- vctrs::field(x, "outcome_reason") 
  oreasy <- vctrs::field(y, "outcome_reason") 
  creasx <- vctrs::field(x, "censor_reason") 
  creasy <- vctrs::field(y, "censor_reason") 
  
  new_rcensored(
    outcome_reason = new_nominal(.levels = union(levels(oreasx), levels(oreasy))),
    censor_reason  = new_nominal(.levels = union(levels(creasx), levels(creasy))),
    .internal_name = get_internal_name(x),
    .context = get_context(x),
    .extra_descriptors = attr(x, "extra_descriptors")
  )
}

#' @rdname casting
#' @inheritParams vctrs::vec_cast
#' @method vec_cast v_rcensored
#' @export
#' @export vec_cast.v_rcensored
vec_cast.v_rcensored <- function(x, to, ...) UseMethod("vec_cast.v_rcensored")

#' @method vec_cast.v_rcensored v_rcensored
#' @export
vec_cast.v_rcensored.v_rcensored <- function(x, to, ...) x

#' @rdname v_rcensored 
#' @export
as.character.v_rcensored <- function(x, ...) {
  as.character(format(x))
}

#' @export
#' @method vec_restore v_rcensored
vec_restore.v_rcensored <- function(x, to, ...) {
  
  # Maintain metainfo
  iname <- attr(to, "internal_name")
  etime <- attr(to, "end_time")
  edesc <- attr(to, "extra_descriptors")
  ctxt  <- get_context(to)
  hold  <- as.list(x)

  out <- 
  new_rcensored(
    time           = hold[["time"]], 
    censored       = hold[["censored"]],
    outcome        = hold[["outcome"]],
    outcome_reason = hold[["outcome_reason"]],
    censor_reason  = hold[["censor_reason"]], 
    .internal_name = iname,
    .data_summary  = data_summary(),
    .context       = ctxt,
    .end_time      = etime,
    .extra_descriptors = edesc
  )

  # Update data summary
  attr(out, "data_summary") <- describe(out, .descriptors = edesc)
  out
}

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
  out <- print_data_summary(vctrs::field(x, "censored"), 
                            "Proportion censored", "proportion")
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

#' Cast a v_rcensored type to a Surv object
#' 
#' Supports (hopefully obviously) \code{type = "right"} in \code{survival::Surv}
#' @param x a \code{\link{v_rcensored}} object
#' @param censor_as_event an indicator to treat censoring as the event of
#'   interest (such as when estimating censoring probabilities). When
#'   \code{TRUE}, all outcomes are treated as a single censoring event and
#'   censoring events as a single outcome. Defaults to \code{FALSE}.
#' @param multiple_endpoint an indicator to treat the outcomes as multiple
#'   endpoint data, otherwise all outcomes are treated as being the same event
#'   type. Defaults to \code{FALSE}.
#' @export

# Note that there doesn't seem to be a generic defined for `as.Surv` in the
# survival package, otherwise we would prefer to create an S3 method here.
as_Surv <- function(x, censor_as_event = FALSE, multiple_endpoint = FALSE){

  stopifnot(
    is_rcensored(x),
    is_truth(censor_as_event),
    is_truth(multiple_endpoint),
    # Multiple endpoints combined with censoring as an event doesn't make
    # sense, so let's throw an error early
    ! (censor_as_event && multiple_endpoint)
  )

  # `as_canonical` converts the `outcome_reason` field to a factor where missing
  # values corresponding to non-events (i.e. censored data)
  x_fct <- as_canonical(get_outcome_reason(x))
  event <- `if`(
    multiple_endpoint,
    as_Surv_events_multiple(x_fct),
    as_Surv_events_single(x_fct, censor_as_event)
  )

  # Cast the `time` field to numeric before invoking `Surv`
  survival::Surv(
    time  = as_canonical(get_time(x)),
    event = event,
    type  = "right"
  )
}

#' Cast an outcome factor to a logical
#' @param x_fct a factor with nonmissing values represent a given event type
#' @inheritParams as_Surv
#' @noRd
as_Surv_events_single <- function(x_fct, censor_as_event) {
  is_censored <- is.na(x_fct)
  `if`(
    censor_as_event,
    is_censored,
    ! is_censored
  )
}

#' Add an explicit missing level for a factor
#' @inheritParams as_Surv_events_single
#' @noRd
as_Surv_events_multiple <- function(x_fct) {

  # Obtain the names of the levels in `x_fct` and ensure that there isn't a name
  # collision with the name we will give to censored observations as given by
  # `na_lvl`
  na_lvl <- "(censored)"
  lvls <- levels(x_fct)
  if (na_lvl %in% lvls) {
    stop(
      "the outcome name ",
      na_lvl,
      " is reserved to indicate censored observations, please rename this outcome.",
      .call = FALSE
    )
  }

  # Add a new level with name `na_lvl` (and make it the first level to conform
  # with `survival::Surv`'s expectations), and convert NAs to this new level.
  # Adapted from `forcats::refactor`.
  new_fct <- factor(x_fct, levels = c(na_lvl, lvls), ordered = FALSE)
  new_fct[is.na(new_fct)] <- na_lvl
  attributes(new_fct) <- utils::modifyList(attributes(x_fct), attributes(new_fct))
  new_fct
}

#' @method sort v_rcensored
#' @export
sort.v_rcensored <- function(x, decreasing = FALSE, ...){
  ord <- order(get_time(x), decreasing = decreasing)
  
  vctrs::field(x, "time")           <- get_time(x)[ord]
  vctrs::field(x, "censored")       <- get_censored(x)[ord]
  vctrs::field(x, "censor_reason")  <- get_censor_reason(x)[ord]
  vctrs::field(x, "outcome")        <- get_outcome(x)[ord]
  vctrs::field(x, "outcome_reason") <- get_outcome_reason(x)[ord]
  x
}

#' Combine Right-Censored Events
#'
#' Create a right-censored event based on the minimum time to event among a
#' collection of right-censored events. In the event of ties for censoring or
#' outcome events, earlier inputs are given precedence over later inputs.
#'
#' @inheritParams v_rcensored
#' @param ... a collection of `v_rcensored` objects.
#' @param new_end_time either one of `"strict"` or `"min"`, or a postive scalar
#'   value. In the case of `"strict"` the inputs are assumed to all have the
#'   same `end_time` value (otherwise an error is thrown), and the `end_time` of
#'   the output object is the same as that of the inputs. In the case of
#'   `"min"`, the minimum `end_time` value among all of the inputs is the taken
#'   to be the `end_time` of the output object. In the case of a positive scalar
#'   input for `new_end_time`, the `end_time` of the output object is taken to
#'   be the value of `new_end_time`. Note that an error is thrown if the value
#'   of `new_end_time` is larger than the smallest `end_time` value among the
#'   inputs since this situation could result in faulty data.
#'
#' @return A `v_rcensored` object.
#' @export

pmin_v_rcensored <- function(...,
                             new_end_time = "strict",
                             internal_name = "",
                             context = methods::new("context"),
                             extra_descriptors = list()) {

  # collect the dots into a list and validate the form of the inputs
  rcen_list <- list(...)
  stopifnot(
    purrr::map_lgl(rcen_list, is_v_rcensored),
    is.character(new_end_time) || check_number_positive(new_end_time)
  )
  # if there are no input `v_rcensored` objects then construct an empty
  # `v_rcensored` object and return early
  if (length(rcen_list) == 0L) {
    out <- v_rcensored(
      end_time          = `if`(is.character(new_end_time), Inf, new_end_time),
      internal_name     = internal_name,
      context           = context,
      extra_descriptors = extra_descriptors
    )
    return(out)
  }

  # extract a representation of the original outcome and censoring times for
  # each input (and noting that "unneeded" information that was shadowed by
  # earlier events or censoring has been discarded), and then combine the
  # elements of the per-input lists across the various inputs
  unpacked_list <- purrr::map(rcen_list, .v_rcensored_unpack)
  unpacked_t_list <- purrr::map(
    purrr::transpose(unpacked_list),
    purrr::flatten
  )

  # compute the end time that will be used for the new variable
  end_time_prev <- unlist(unpacked_t_list$end_time)
  etime_dbl <- `if`(
    is.character(new_end_time),
    pmin_v_rcensored_endtime_inputchr(end_time_prev, new_end_time),
    pmin_v_rcensored_endtime_inputdbl(end_time_prev, new_end_time)
  )

  v_rcensored(
    outcomes          = unpacked_t_list$outcomes,
    censors           = unpacked_t_list$censors,
    end_time          = etime_dbl,
    internal_name     = internal_name,
    context           = context,
    extra_descriptors = extra_descriptors
  )
}

#' Obtain An End Time Value for a Numeric New Event Time Input
#'
#' @param end_time_prev a vector of positive values.
#' @param new_end_time a positive scalar value.
#' @return a positive scalar value.
#' @noRd
pmin_v_rcensored_endtime_inputdbl <- function(end_time_prev, new_end_time) {
  stopifnot(new_end_time <= end_time_prev)
  new_end_time
}

#' Obtain An End Time Value for a Character New Event Time Input
#'
#' @param end_time_prev a vector of positive values.
#' @param new_end_time a string.
#' @return a positive scalar value.
#' @noRd
pmin_v_rcensored_endtime_inputchr <- function(end_time_prev, new_end_time) {
  stopifnot(new_end_time %in% c("strict", "min"))
  switch(
    EXPR = new_end_time,
    strict = {
      stopifnot(end_time_prev == end_time_prev[1L])
      end_time_prev[1L]
    },
    min = {
      min(end_time_prev)
    }
  )
}

#' Get elements from v_rcensored
#' 
#' @name v_rcensored_accessors
#' @param x a \code{\link{v_rcensored}} vector
#' @importFrom vctrs field
NULL

get_from_rcensored <- function(x, what){
  vctrs::field(x, what)
}

#' @rdname v_rcensored_accessors
#' @export
get_time <- function(x){
  get_from_rcensored(x, "time")
}

#' @rdname v_rcensored_accessors
#' @export
get_censored <- function(x){
  get_from_rcensored(x, "censored")
}

#' @rdname v_rcensored_accessors
#' @export
get_censor_reason <- function(x){
  get_from_rcensored(x, "censor_reason")
}

#' @rdname v_rcensored_accessors
#' @export
get_outcome <- function(x){
  get_from_rcensored(x, "outcome")
}

#' @rdname v_rcensored_accessors
#' @export
get_outcome_reason <- function(x){
  get_from_rcensored(x, "outcome_reason")
}
