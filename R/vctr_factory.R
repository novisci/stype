
# # Create 
# # font   : new_vctr
# # 
# # helpers : class, is_<class>, setOldClass
# # format:  format.<class>, vec_ptype_abbr.<class>
# # cast/coerce:

new_cached_sum <- function(x = double(), sum = 0L) {
   vec_assert(x,   ptype = double())
   vec_assert(sum, ptype = double(), size = 1L)

   new_vctr(x, sum = sum, class = "vctrs_cached_sum")
}

cached_sum <- function(x) {
  x <- vec_cast(x, double())
  new_cached_sum(x, sum(x))
}

obj_print_footer.vctrs_cached_sum <- function(x, ...) {
   cat("# Sum: ", format(attr(x, "sum"), digits = 3), "\n", sep = "")
}

vec_math.vctrs_cached_sum <- function(fun, x, ...) {
   cat("Using cache\n")
   switch(fun,
          sum  = attr(x, "sum"),
          mean = attr(x, "sum") / length(x),
          vec_math_base(fun, x, ...)
   )
}

vec_restore.vctrs_cached_sum <- function(x, to, ..., i = NULL) {
   new_cached_sum(x, sum(x))
}

class(x)
x <- cached_sum(c(1:100))
x

y <- tibble::tibble(x = cached_sum(c(1:100)))
library(dplyr)
y %>% filter(rep(c(TRUE, FALSE), 50)) -> z

y$x
z$x

