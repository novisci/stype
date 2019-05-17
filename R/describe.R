
setGeneric("descriptor", function(x) standardGeneric("descriptor"))

setMethod(
  f          = "descriptor",
  signature  = "variable",
  definition = function(x){
    append(
      list(
        # TODO insert useful summary stats that apply across all variables here
        has_missing = anyNA
      ),
      # TODO create methods for each data type below
      descriptor(x@.Data))
  }
)

setMethod(
  f          = "descriptor",
  signature  = "logical",
  definition = function(x){
    list(
      proportion = function(x) sum(x, na.rm = TRUE)
    )
  }
)


setGeneric("describe", function(x, g, w, ...) standardGeneric("describe"))

setMethod(
  f          = "describe",
  signature  = c("variable", "missing", "missing"),
  definition = function(x, g, w, ...){
    desc <- descriptor(x)
    out  <-  purrr::map(
      .x = desc,
      .f = ~ .x(x))
    setNames(out, nm = names(desc))
    out
  }
)

