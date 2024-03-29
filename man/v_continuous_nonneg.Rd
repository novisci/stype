% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/v_continuous_nonneg.R
\name{v_continuous_nonneg}
\alias{v_continuous_nonneg}
\alias{nneg}
\alias{is_continuous_nonneg}
\alias{is_nonneg}
\alias{as_nonneg_continuous}
\alias{as_continuous_nonneg}
\alias{as_canonical.v_continuous_nonneg}
\title{Non-negative continuous vectors}
\usage{
v_continuous_nonneg(
  x = ptypeFUN(),
  internal_name = "",
  context,
  auto_compute_summary = auto_compute_default,
  extra_descriptors = list()
)

nneg(
  x = ptypeFUN(),
  internal_name = "",
  context,
  auto_compute_summary = auto_compute_default,
  extra_descriptors = list()
)

is_continuous_nonneg(x)

is_nonneg(x)

as_nonneg_continuous(x)

as_continuous_nonneg(x)

\method{as_canonical}{v_continuous_nonneg}(x)
}
\arguments{
\item{x}{nonnegative \code{double}}

\item{internal_name}{the internal name of the variable}

\item{context}{a \code{\link{context}}}

\item{auto_compute_summary}{an indicator of whether the \code{data_summary} is
automatically computed whenever a vector is initialized, subset, or
concatenated. Defaults to \code{TRUE}. If this option is set to \code{FALSE},
then \code{\link{get_data_summary}} is the only way to compute the summary.
The \code{\link{data_summary_l}} lens will return an empty \code{data_summary}.}

\item{extra_descriptors}{A \code{list} of \code{\link{descriptors}} functions
appended to the default \code{\link{descriptors}}.}
}
\description{
Constructors and methods for a nonnegative continuous data type.
\code{v_continuous_nonneg} and \code{nneg} are synonyms that each create a
new \code{v_continuous_nonneg} object subclassed from \code{v_continuous} and
\code{vctrs_vctr}. \cr\cr
Support: \eqn{\mathbf{R^+}}{the nonnegative Reals}* (plus \code{\link{NA_real_}}) \cr
Prototype: \code{\link{double}} \cr
\emph{}* - i.e. floating-point number
}
\examples{
# Example data
src_dbl <- c(1, 2.5, 4, 5.5, NA_real_)

# Constructor for the `v_continuous_nonneq` class. One can also use `nneg`
# which is a synonym for the `v_continuous_nonneg` function.
v <- v_continuous_nonneg(
  x = src_dbl,
  internal_name = "v_example",
  context = context(
    short_label = "important_var",
    long_label  = "Very important variable"
  ),
  extra_descriptors = list()
)

# Helper functions and methods
is_continuous_nonneg(v)
as_nonneg_continuous(src_dbl)
as.character(v)
as_canonical(v)
}
\seealso{
Other stype types: 
\code{\link{tbl_analysis}},
\code{\link{v_binary}},
\code{\link{v_continuous}},
\code{\link{v_count}},
\code{\link{v_nominal}},
\code{\link{v_ordered}},
\code{\link{v_proportion}},
\code{\link{v_rcensored}}
}
\concept{stype types}
