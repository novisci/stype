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
#'    interest. Defaults to \code{FALSE}. When \code{TRUE}, the first outcome is 
#'    treated as the outcome of interest and the others as competing risks. When
#'    \code{FALSE}, all outcomes are treated as a single censoring event and 
#'    censoring events as a single outcome.
#' @export
as_Surv <- function(x, censor_as_event = FALSE){
  hold <- as.integer(as_canonical(vctrs::field(x, "outcome_reason")))
  hold[is.na(hold)] <- 0L
  
  # TODO: how should administrative censoring be handled here?
  event <- `if`(
    censor_as_event,
    hold == 0L,
    as.factor(hold)
  )
  
  survival::Surv(
    time  = as_canonical(vctrs::field(x, "time")),
    event = event,
    type  = "right"
  )
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
