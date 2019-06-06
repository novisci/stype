#' example
#' 
#'  Some desc
#' 
#' @name example
#' @importFrom methods setOldClass
#' @export


new_example <- function(x = double(), sum = 0L, mean = 0L){
   vctrs::vec_assert(x, ptype = double())
   vctrs::vec_assert(sum, ptype = double(), size = 0L)
   vctrs::vec_assert(mean, ptype = NULL, size = NULL)
     
   vctrs::new_vctr(x, sum = sum, mean = mean, class = 'vctrs_example')
}


methods::setOldClass(c("vctrs_example", "vctrs_vctr"))

#' @describeIn example constructor function for example objects
#' @export

example <- function(x = double()){
  x <- vctrs::vec_cast(x, double())
  new_example(x)
}

#' @describeIn example predicate function for example objects
#' @export
is_example <- function(x){
  inherits(x, "vctrs_example")
}

# Formatting of example vectors

format.vctrs_example <- function(x, ...) {
  apply_formatters(x, c('round','as.character'), ...)
}

# Casting and coercing ####

# Boilerplate from:
# https://vctrs.r-lib.org/articles/s3-vector.html#double-dispatch
vec_type2.example <- function(x, y, ...) UseMethod("vec_type2.example", y)
vec_type2.example.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}
vec_type2.example.vctrs_unspecified <- function(x, y, ...) x

vec_cast.example <- function(x, to) UseMethod("vec_cast.example")
vec_cast.example.default <- function(x, to) vec_default_cast(x, to)

vec_type2.vctrs_example.vctrs_example <- function(x, y, ...) new_example()

## coercing from
# Note: "Because double dispatch is a bit of a hack, we need to provide two methods.
# It’s your responsibility to ensure that each pair return the same result: 
# if they don’t you will get weird and unpredictable behaviour."

vec_type2.vctrs_example.double <- function(x, y, ...) double()
vec_type2.double.vctrs_example <- function(x, y, ...) double()

vec_type2.vctrs_example.numeric <- function(x, y, ...) numeric()
vec_type2.numeric.vctrs_example <- function(x, y, ...) numeric()

# coercing a example to a example
vec_cast.vctrs_example.vctrs_example <- function(x, to) x

vec_cast.vctrs_example.double <- function(x, y, ...) example()
vec_cast.double.vctrs_example <- function(x, y, ...) vec_data()

vec_cast.vctrs_example.numeric <- function(x, y, ...) example()
vec_cast.numeric.vctrs_example <- function(x, y, ...) vec_data()

as_example <- function(x) {
  vec_cast(x, new_example())
}
#' example2
#' 
#'  Some desc 2
#' 
#' @name example2
#' @importFrom methods setOldClass
#' @export


new_example2 <- function(){
   vctrs::vec_assert(x, ptype = numeric())
   
     
   vctrs::new_vctr(x,  class = 'vctrs_example2')
}


methods::setOldClass(c("vctrs_example2", "vctrs_vctr"))

#' @describeIn example2 constructor function for example2 objects
#' @export

example2 <- function(x = numeric()){
  x <- vctrs::vec_cast(x, numeric())
  new_example2(x)
}

#' @describeIn example2 predicate function for example2 objects
#' @export
is_example2 <- function(x){
  inherits(x, "vctrs_example2")
}

# Formatting of example2 vectors

format.vctrs_example2 <- function(x, ...) {
  apply_formatters(x, c('round','as.character'), ...)
}

# Casting and coercing ####

# Boilerplate from:
# https://vctrs.r-lib.org/articles/s3-vector.html#double-dispatch
vec_type2.example2 <- function(x, y, ...) UseMethod("vec_type2.example2", y)
vec_type2.example2.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}
vec_type2.example2.vctrs_unspecified <- function(x, y, ...) x

vec_cast.example2 <- function(x, to) UseMethod("vec_cast.example2")
vec_cast.example2.default <- function(x, to) vec_default_cast(x, to)

vec_type2.vctrs_example2.vctrs_example2 <- function(x, y, ...) new_example2()

## coercing from
# Note: "Because double dispatch is a bit of a hack, we need to provide two methods.
# It’s your responsibility to ensure that each pair return the same result: 
# if they don’t you will get weird and unpredictable behaviour."



# coercing a example2 to a example2
vec_cast.vctrs_example2.vctrs_example2 <- function(x, to) x



as_example2 <- function(x) {
  vec_cast(x, new_example2())
}
