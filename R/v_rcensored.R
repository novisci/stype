#' Right censored vectors
#'
#' Constructors and methods for a right-censored data type. \code{v_rcensored}
#' and \code{rcen} are synonyms that each create a new \code{v_rcensored} object
#' subclassed from \code{censored}, \code{vctrs_rcrd}, and \code{vctrs_vctr}.
#' Unlike other representations, such the `survival` package's `Surv` object,
#' `v_rcensored` can be subset with `[` and concatenated with `c` as you would
#' any other vector. The type is implemented as a \code{\link[vctrs]{new_rcrd}}
#' where the necessary data are contained in \code{\link[vctrs]{fields}}.
#'
#' `as_canonical` casts the vector to a `list`. See \code{\link{v_rcensored_accessors}}
#' for functions to access components of a `v_rcensored`.
#'
#' When printed, an open right triangle indicates an observation was censored.
#' A closed right triangle indicates an observation reached `end_time` without
#' being censored or having an outcome. No triangle indicates an observation
#' that has at least one of the outcomes.
#'
#' When constructing `v_rcensored`, the input must not contain `NA` values.
#' Use `Inf` to indicate that an event has not been observed.
#'
#' @name v_rcensored
#' @importFrom methods setOldClass
#' @importFrom vctrs vec_cast vec_ptype2 vec_data new_vctr vec_assert vec_arith_base
#' @inheritParams v_binary
#' @family stype types
#' @examples
#' # Example censoring times data
#' ctimeA <- v_continuous_nonneg(c(5, 6, 10, 1, Inf, 19), internal_name = "cA")
#' ctimeB <- v_continuous_nonneg(c(4, 1, 15, Inf, Inf, 21), internal_name = "cB")
#'
#' # Example outcome times data
#' otimeA <- v_continuous_nonneg(c(2, 6, 11, 12, Inf, 25), internal_name = "oA")
#' otimeB <- v_continuous_nonneg(c(1, Inf, 10, Inf, Inf, 23), internal_name = "oB")
#'
#' # Constructor for the `v_rcensored` class. One can also use `rcen` which is a
#' # synonym for the `v_rcensored` function.
#' v <- v_rcensored(
#'   outcomes = list(ctimeA, ctimeB),
#'   censors = list(otimeA, otimeB),
#'   end_time = 15,
#'   internal_name = "v_example",
#'   context = context(
#'     short_label = "important_var",
#'     long_label  = "Very important variable"
#'   ),
#'   extra_descriptors = list()
#' )
#'
#' # Helper functions and methods
#' is_rcensored(v)
#' as.character(v)
#' as_canonical(v)
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
new_rcensored <- function(time = v_continuous_nonneg(auto_compute_summary = .auto_compute_summary),
                          censored = v_binary(auto_compute_summary = .auto_compute_summary),
                          outcome = v_binary(auto_compute_summary = .auto_compute_summary),
                          censor_reason = v_nominal(auto_compute_summary = .auto_compute_summary),
                          outcome_reason = v_nominal(auto_compute_summary = .auto_compute_summary),
                          .end_time = numeric(),
                          .internal_name = character(),
                          .data_summary = data_summary(),
                          .context = context(),
                          .auto_compute_summary = auto_compute_default,
                          .extra_descriptors = list()) {
  # Check types of inputs
  vctrs::vec_assert(
    time,
    ptype = v_continuous_nonneg(auto_compute_summary = .auto_compute_summary)
  )

  vctrs::vec_assert(
    censored,
    ptype = v_binary(auto_compute_summary = .auto_compute_summary)
  )

  vctrs::vec_assert(
    outcome,
    ptype = v_binary(auto_compute_summary = .auto_compute_summary)
  )

  vdat <- list(
    time = time,
    censored = censored,
    outcome = outcome,
    censor_reason = censor_reason,
    outcome_reason = outcome_reason
  )

  new_stype_vctr(
    vdat,
    .internal_name = .internal_name,
    .data_summary  = .data_summary,
    .context       = .context,
    .auto_compute_summary = .auto_compute_summary,
    .extra_descriptors = .extra_descriptors,
    .class = c("v_rcensored", "censored"),
    new_fun = vctrs::new_rcrd,
    end_time = .end_time
  )
}

#' @importFrom methods setOldClass
methods::setOldClass(c("v_rcensored", "v_censored", "vctrs_vctr"))

#' Right-censored constructor
#'
#' @details
#'  The levels/labels follow the following rules:
#'  1. If a named list (all elements must be uniquely named) is passed to
#'     `outcomes` (or `censors`) and all the vectors have short labels,
#'     then the list names are the levels and the short labels become the labels.
#'  2. If a named list is passed to `outcomes` (or `censors`) and *any* of the
#'     vectors are missing short labels, then the list names become the levels
#'     and the labels.
#'  3. If a unnamed list is passed and all the vectors have internal names and
#'     all the vectors have short labels, then the internal names are the levels
#'    and the short labels become the labels.
#'  4. If a unnamed list is passed and all the vectors have internal names *any*
#'     of the vectors are missing short labels, then the internal names become
#'     the levels and  labels.
#'  5. Otherwise, `as.character(1:length(x))` become the levels and labels,
#'     where `length(x)` is the number of list elements.
#'
#' @param outcomes Either a \code{v_continuous_nonneg} vector or a \code{list} of
#' \code{v_continuous_nonneg} vectors that define the outcomes. The order of this list
#' defines the precedence of outcomes. That is,if the first outcome and second
#' outcome occur at the same time, the first outcome is the reason for the outcome.
#' Use `Inf` to indicate that an event has not been observed.
#' @param censors A \code{list} of \code{v_continuous_nonneg} vectors that define the
#' censor The order of this list defines the precedence of censoring That is,
#' if the first censor and second censor occur at the same time, the first
#' censor is the reason for the censoring. Use `Inf` to indicate that an event
#' has not been observed.
#' @param end_time A \code{numeric} scalar defining the end of follow-up.
#' @param extra_descriptors A \code{list} of \code{\link{descriptors}} functions
#'        appended to the default \code{\link{descriptors}}.
#' @importFrom vctrs vec_recycle vec_size
#' @rdname v_rcensored
#' @export
v_rcensored <- function(outcomes = v_continuous_nonneg(),
                        censors,
                        end_time = Inf,
                        internal_name = "",
                        context,
                        auto_compute_summary = auto_compute_default,
                        extra_descriptors = list()) {
  if (is_continuous_nonneg(outcomes)) {
    outcomes <- list(outcomes)
  }


  assertthat::assert_that(
    length(outcomes) > 0 && !missing(outcomes),
    msg = "You must provide at least one outcome to v_rcensored."
  )

  assertthat::assert_that(
    all(length(outcomes[[1]]) == purrr::map_int(outcomes, length)),
    msg = "All outcomes must have the same length."
  )

  assertthat::assert_that(
    is_truth(auto_compute_summary),
    msg = "auto_compute_summary must be TRUE or FALSE."
  )

  if (!missing(censors)) {
    assertthat::assert_that(
      all(length(censors[[1]]) == purrr::map_int(censors, length)),
      msg = "All censors must have the same length."
    )

    assertthat::assert_that(
      (all(length(outcomes[[1]]) == purrr::map_int(censors, length))),
      msg = "All censors must have the same length as the outcomes."
    )
  }

  if (missing(censors)) {
    censors <- vctrs::vec_recycle(
      v_continuous_nonneg(Inf),
      if (vctrs::vec_size(outcomes) == 0L) {
        0L
      } else {
        vctrs::vec_size(outcomes[[1]])
      }
    )
  }

  if (is_continuous_nonneg(censors)) {
    censors <- list(censors)
  }

  purrr::walk(
    .x = vctrs::vec_c(outcomes, censors),
    .f = ~ {
      assertthat::assert_that(
        is_continuous_nonneg(.x),
        msg = "Input vectors must be v_continuous_nonneg types."
      )
    }
  )

  purrr::walk(
    .x = vctrs::vec_c(outcomes, censors),
    .f = ~ {
      assertthat::assert_that(
        !anyNA(.x),
        msg = c(
          "Input vectors must not contain any NA values.",
          "Use Inf to indicate that an event has not yet been observed."
        )
      )
    }
  )

  # End time must be scalar
  vctrs::vec_assert(end_time, ptype = numeric(), size = c(1L))

  vdat <- .v_rcensored(outcomes, censors, end_time,
    auto_compute_summary = auto_compute_summary
  )

  if (missing(context)) {
    context <- methods::new("context")
  }

  out <- new_rcensored(
    time = vdat[["times"]],
    censored = vdat[["censored"]],
    outcome = vdat[["outcome"]],
    outcome_reason = vdat[["outcome_reason"]],
    censor_reason = vdat[["censor_reason"]],
    .end_time = end_time,
    .internal_name = check_internal_name(internal_name),
    .data_summary = data_summary(),
    .context = context,
    .auto_compute_summary = auto_compute_summary,
    .extra_descriptors = extra_descriptors
  )

  dsum <-
    `if`(
      auto_compute_summary,
      describe(out, .descriptors = extra_descriptors),
      data_summary()
    )
  attr(out, "data_summary") <- dsum
  out
}

#' @rdname v_rcensored
#' @export
rcen <- v_rcensored

#' Predicate function for right-censored objects
#' @rdname v_rcensored
#' @export
is_rcensored <- function(x) {
  inherits(x, "v_rcensored")
}

#' Internal function for finding the times/reasons for a right censored stype
#' @param ... TODO
#' @noRd
gather_time_reason <- function(...) {
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
gather_times_reasons <- function(times, levs, auto_compute_summary) {
  itimes <- purrr::map(times, as_canonical)

  hold <- purrr::pmap(
    .l = itimes,
    .f = gather_time_reason
  )

  list(
    times = v_continuous_nonneg(purrr::map_dbl(hold, "time"),
      auto_compute_summary = auto_compute_summary
    ),
    # reasons = factor(levs[purrr::map_dbl(hold, "reason") + 1], levels = levs)
    reasons = v_nominal(factor(levs[purrr::map_dbl(hold, "reason") + 1],
      levels = levs
    ), auto_compute_summary = auto_compute_summary)
  )
}

#' Internal function for getting levels and labels for censoring and outcomes
#'
#' @noRd
get_levels_labels <- function(x) {
  lvls <- character(length(x))
  lbls <- character(length(x))

  xnames <- names(x)
  inames <- purrr::map_chr(x, ~ get_internal_name(.x))
  slabls <- purrr::map_chr(x, ~ get_short_label(.x))

  has_all_xnames <- !is.null(xnames) &&
    length(xnames) == length(unique(xnames)) &&
    all(xnames != "")
  has_all_inames <- all(inames != "")
  has_all_labels <- all(slabls != "")

  if (has_all_xnames & has_all_labels) {
    lvls <- xnames
    lbls <- slabls
  } else if (has_all_xnames & !has_all_labels) {
    lvls <- lbls <- xnames
  } else if (has_all_inames & has_all_labels) {
    lvls <- inames
    lbls <- slabls
  } else if (has_all_inames & !has_all_labels) {
    lvls <- lbls <- inames
  } else {
    lvls <- lbls <- as.character(1:length(x))
  }

  list(levels = lvls, labels = lbls)
}


#' Internal Right-censored constructor
#' @noRd
.v_rcensored <- function(outcomes, censors, end_time, auto_compute_summary) {
  clev <- get_levels_labels(censors)
  olev <- get_levels_labels(outcomes)

  cens <- gather_times_reasons(censors, c("not_censored", clev[["levels"]]),
    auto_compute_summary = auto_compute_summary
  )
  outs <- gather_times_reasons(outcomes, c("not_outcome", olev[["levels"]]),
    auto_compute_summary = auto_compute_summary
  )

  hold <- gather_times_reasons(
    append(
      list(outs[["times"]], cens[["times"]]),
      list(v_continuous_nonneg(end_time))
    ),
    levs = c("none", "outcome", "censor", "admin"),
    auto_compute_summary = auto_compute_summary
  )

  creas <- cens[["reasons"]]
  creas[hold[["reasons"]] != "censor"] <- NA
  creas <- factor(as.character(creas),
    levels = clev[["levels"]],
    labels = clev[["labels"]]
  )

  oreas <- outs[["reasons"]]
  oreas[hold[["reasons"]] != "outcome"] <- NA
  oreas <- factor(as.character(oreas),
    levels = olev[["levels"]],
    labels = olev[["labels"]]
  )

  list(
    times = hold[["times"]],
    censored = v_binary(hold[["reasons"]] == "censor", auto_compute_summary = auto_compute_summary),
    outcome = v_binary(hold[["reasons"]] == "outcome", auto_compute_summary = auto_compute_summary),
    censor_reason = v_nominal(creas, auto_compute_summary = auto_compute_summary),
    outcome_reason = v_nominal(oreas, auto_compute_summary = auto_compute_summary)
  )
}

#' The invers-ish of .v_rcensored
#' @importFrom vctrs field
#' @importFrom purrr set_names
#' @noRd
.v_rcensored_unpack <- function(x) {
  time <- vctrs::field(x, "time")

  oreas <- vctrs::field(x, "outcome_reason")
  ocomes <- purrr::map(
    .x = purrr::set_names(levels(oreas)),
    .f = ~ {
      hold <- time
      idx <- oreas == .x
      hold[!ifelse(is.na(idx), FALSE, idx)] <- Inf
      attr(hold, "internal_name") <- .x
      hold
    }
  )

  creas <- vctrs::field(x, "censor_reason")
  censr <- purrr::map(
    .x = purrr::set_names(levels(creas)),
    .f = ~ {
      hold <- time
      idx <- creas == .x
      hold[!ifelse(is.na(idx), FALSE, idx)] <- Inf
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

# Casting and coercing ####

# Casting and coercing ####
#' @rdname casting
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

  auto <- decide_auto_compute(x, y)
  new_rcensored(
    outcome_reason =
      new_nominal(
        .levels = union(levels(oreasx), levels(oreasy)),
        .auto_compute_summary = auto
      ),
    censor_reason = new_nominal(
      .levels = union(levels(creasx), levels(creasy)),
      .auto_compute_summary = auto
    ),
    .internal_name = get_internal_name(x),
    .context = get_context(x),
    .auto_compute_summary = auto,
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
  # browser()
  # Maintain metainfo
  iname <- attr(to, "internal_name")
  etime <- attr(to, "end_time")
  edesc <- attr(to, "extra_descriptors")
  ctxt <- get_context(to)
  hold <- as.list(x)
  auto <- attr(to, "auto_compute_summary")

  out <-
    new_rcensored(
      time = hold[["time"]],
      censored = hold[["censored"]],
      outcome = hold[["outcome"]],
      outcome_reason = hold[["outcome_reason"]],
      censor_reason = hold[["censor_reason"]],
      .internal_name = iname,
      .data_summary = data_summary(),
      .context = ctxt,
      .end_time = etime,
      .auto_compute_summary = auto,
      .extra_descriptors = edesc
    )

  # Update data summary
  dsum <-
    `if`(
      auto,
      describe(out, .descriptors = edesc),
      data_summary()
    )
  attr(out, "data_summary") <- dsum
  out
}

#' @rdname v_rcensored
#' @export
as_canonical.v_rcensored <- function(x) {
  as.list(vctrs::vec_data(x))
}

#' @importFrom vctrs obj_print_footer
#' @importFrom crayon "%+%"
#' @method obj_print_footer v_rcensored
#' @export
obj_print_footer.v_rcensored <- function(x, ...) {
  print_footer(
    vctrs::field(x, "censored"),
    stats = list(proportion = list(
      label = "Proportion censored",
      printer = print_numeric_summary
    ))
  )
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

#' Cast a \code{v_rcensored} Type to a Surv Object
#'
#' Useful for converting a \code{v_rcensored} object into the form that is
#' expected as an input to various routines in the survival package. More
#' concretely, this function returns an object with the same form as is returned
#' by calling \code{\link[survival:Surv]{survival::Surv()}}.
#'
#' @param x a \code{\link{v_rcensored}} object.
#' @param censor_as_event an indicator for whether to treat censoring as the
#'   event of interest (such as when estimating censoring probabilities). When
#'   \code{TRUE}, all outcomes are treated as a single censoring event and
#'   censoring events as a single outcome. Defaults to \code{FALSE}.
#' @param multiple_endpoint an indicator for whether to treat the outcomes as
#'   multiple endpoint data. Otherwise all outcomes are treated as being the
#'   same event type. Defaults to \code{FALSE}.
#' @examples
#'
#' # Example censoring and outcome times data
#' ctime <- v_continuous_nonneg(c(5, 6, 10, 1, Inf, 19), internal_name = "censoring")
#' otime <- v_continuous_nonneg(c(2, 6, 11, 12, Inf, 25), internal_name = "outcomes")
#'
#' # Construct a `v_censored` value
#' v <- v_rcensored(
#'   outcomes = list(ctime),
#'   censors = list(otime),
#'   end_time = 15,
#' )
#'
#' # Convert to a 'survival' package right-censored data representation
#' as_Surv(v)
#' @export

# Note that there doesn't seem to be a generic defined for `as.Surv` in the
# survival package, otherwise we would prefer to create an S3 method here.
as_Surv <- function(x, censor_as_event = FALSE, multiple_endpoint = FALSE) {
  stopifnot(
    is_rcensored(x),
    is_truth(censor_as_event),
    is_truth(multiple_endpoint),
    # Multiple endpoints combined with censoring as an event doesn't make
    # sense, so let's throw an error early
    !(censor_as_event && multiple_endpoint)
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
    !is_censored
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
sort.v_rcensored <- function(x, decreasing = FALSE, ...) {
  ord <- order(get_time(x), decreasing = decreasing)

  vctrs::field(x, "time") <- get_time(x)[ord]
  vctrs::field(x, "censored") <- get_censored(x)[ord]
  vctrs::field(x, "censor_reason") <- get_censor_reason(x)[ord]
  vctrs::field(x, "outcome") <- get_outcome(x)[ord]
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
    purrr::map_lgl(rcen_list, is_rcensored),
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

get_from_rcensored <- function(x, what) {
  vctrs::field(x, what)
}

#' @rdname v_rcensored_accessors
#' @export
get_time <- function(x) {
  get_from_rcensored(x, "time")
}

#' @rdname v_rcensored_accessors
#' @export
get_censored <- function(x) {
  get_from_rcensored(x, "censored")
}

#' @rdname v_rcensored_accessors
#' @export
get_censor_reason <- function(x) {
  get_from_rcensored(x, "censor_reason")
}

#' @rdname v_rcensored_accessors
#' @export
get_outcome <- function(x) {
  get_from_rcensored(x, "outcome")
}

#' @rdname v_rcensored_accessors
#' @export
get_outcome_reason <- function(x) {
  get_from_rcensored(x, "outcome_reason")
}
