## Generate vctr classes
unlink("R/vctr_classes.R")

template <- readLines("R/vctr_template")
data <-
  list(
    generate_template_arglist(
      class = "example",
      ptype = "double",
      desc  = " Some desc",
      formatters = c("round", "as.character"),
      attrs      = list(
        list(NAME = "sum", PTYPE = "double", SIZE = "0L", DEFAULT = "0L"),
        list(NAME = "mean", DEFAULT = "0L")
      ),
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
      formatters = c("round", "as.character")
    )
      
    #   COMMON_TYPES = generate_type_rules(
    #     class = "example2",
    #     list(TYPE = "double", TO = "double"),
    #     list(TYPE = "numeric", TO = "numeric")),
    #   
    #   ## Casting rules
    #   CASTING = generate_cast_rules(
    #     class = "example2",
    #     list(TYPE = "double",  TO = "vec_data"),
    #     list(TYPE = "numeric", TO = "vec_data"))
    # )
  )

purrr::walk(
  .x = data,
  .f = ~ cat(whisker.render(template, .x), file = "R/vctr_classes.R", append = TRUE)
)

