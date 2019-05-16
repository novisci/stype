
#'
#'

variable <- setClass(
  "variable", 
  slots    = c(
    "name"         = "character",
    "short_label"  = "character",
    "long_label"   = "character",
    "has_missing"  = "logical",
    "type"         = "character",
    "description"  = "description"),
  contains = "vector"
)

setMethod(
  f          = "initialize",
  signature  = "variable",
  definition = function(.Object, ...){

    # Hijack the user's ability to (incorrectly) set has_missing
    # e.g. variable(c(TRUE, NA), has_missing = FALSE)
    dots <- list(...)
    dots[["has_missing"]] <- anyNA(..1)
    
    # browser()
    # if(!("short_label" %in% names(dots))){
    #   dots[["short_label"]] <- dots[["name"]]
    # }
    

    do.call("callNextMethod", args = append(list(.Object), dots))
  })

setValidity(
  Class  = "variable",
  method = function(object){
    
    valid_classes <- c("numeric", "logical", "factor", "character", "Date")
    data_class    <- class(methods::getDataPart(object))
    
    assertthat::validate_that(
      data_class %in% valid_classes,
      msg = sprintf("\nThe data of a variable must be one of %s, not %s.",
                    paste(valid_classes, collapse = ", "),
                    data_class)
    )
    
    purrr::walk(
      .x = c("name", "short_label"),
      .f = function(x){
        assertthat::validate_that(
          length(methods::slot(object, x)) == 1,
          msg = "short_label must be length 1"
        )
      }
    )
    
    TRUE

    
  }
)


test <- variable(c(TRUE, NA), name = "myVar", short_label = "hello")
test
class(test)
typeof(test)


#'
#'

setMethod(
  f          = "show",
  signature  = "variable",
  definition = function(object){
    cat(object@short_label,"\n", object@.Data)
  }
)

#'
#'

outcome <- setClass(
  "outcome",
  slots   =  c("type" = "character"),
  contains = "variable"
)

#'
#'

covariate <- setClass(
  "covariate",
  contains = "variable"    
)


setClassUnion("variableUnion", members = c("outcome", "covariate"))
