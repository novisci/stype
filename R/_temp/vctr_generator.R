## Generate vctr classes
unlink("R/vctr_classes.R")

template <- readLines("R/vctr_template")
data <-
  list(
    generate_template_arglist(
      class = "example",
      ptype = "double",
      desc  = " Some desc",
      descriptors = c("describe_mean_var", "describe_quantiles"),
      formatters = c("round"),
      type_rules = list(
          list(TYPE = "double", TO = "double"),
          list(TYPE = "numeric", TO = "numeric")
      ),
      cast_rules = list(
        list(TYPE = "double",  TO = "vec_data"),
        list(TYPE = "numeric", TO = "vec_data")
      )
    ),
    generate_template_arglist(
      class = "example2",
      ptype = "numeric",
      desc  = " Some desc 2",
      formatters = c("round")
    )
  )

purrr::walk(
  .x = data,
  .f = ~ cat(whisker::whisker.render(template, .x),
             file = "R/vctr_classes.R", append = TRUE)
)

