% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{stop_invalid_math}
\alias{stop_invalid_math}
\title{Throw 'stype_invalid_math' Error}
\usage{
stop_invalid_math(x, op, class = NULL, ...)
}
\arguments{
\item{x}{stype vector}

\item{op}{the math operation}

\item{class}{optional class argument passed to \code{stype_abort}}

\item{...}{additional arguments passed to \code{stype_abort}}
}
\description{
Helper function used to throw an error with a standardized class for the case
where an unsupported math operation is called for a given stype type. This is
intended to be explicitly invoked by internal routines in situations where we
don't want the S3 dispatch to fall through to vctrs handling of math
operators, which amounts to invoking the operator in base with the raw data
as the input for types that are supported, and then restoring the appropriate
vctrs but not stype type (for types that are not supported by vctrs an error
is thrown immediately).
}
\keyword{internal}
