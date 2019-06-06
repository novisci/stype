generate_arg_string <- function(x){
  sprintf("c('%s'),", paste(x, collapse = "','"))
}

to_function_string <- function(x){
  sprintf("%s()", x)
}

new_class_template <- "
new_{{CLASS}} <- function({{SIGNATURE}}){
   vctrs::vec_assert(x, ptype = {{PTYPE}}())
   {{{ATTR_ASSERTS}}}
     
   vctrs::new_vctr(x, {{ATTRS}} class = 'vctrs_{{CLASS}}')
}
"

generate_new_vctr_class <- function(class, ptype, attrs){
  
  CLASS <- class
  PTYPE <- ptype
  
  attr_proto <- list(NAME = "NULL", PTYPE = "NULL", SIZE = "NULL", DEFAULT = "NULL")
  
  attrs <- purrr::map(
    .x = attrs,
    .f = function(x){
      hold <- purrr::list_modify(attr_proto, !!! x)
      print(hold)
      if(hold$PTYPE != "NULL") {
        hold$PTYPE <- to_function_string(hold$PTYPE)
      }
      hold
    }
  )
  
  # generate attr assertions
  ATTR_ASSERTS <- attrs %>%
    purrr::map(
      .f = ~ glue::glue_data(.x, "vctrs::vec_assert({NAME}, ptype = {PTYPE}, size = {SIZE})")
    ) %>%
    glue::glue_collapse(sep = "\n   ")
  
  # generate function signature
  SIGNATURE <- attrs %>%
    purrr::map(
      .f = ~ glue::glue_data(.x, "{NAME} = {DEFAULT}")
    ) %>%
    glue::glue_collapse(sep = ", ") %>%
    sprintf("x = %s, %s", to_function_string(ptype), .)
  
  # generate attr signature of vctrs::new_vctr 
  ATTRS <- attrs %>%
    purrr::map(
      .f = ~ glue::glue_data(.x, "{NAME} = {NAME}")
    ) %>%
    glue::glue_collapse(sep = ", ") %>%
    sprintf("%s,", .)
  
  whisker::whisker.render(
    template = new_class_template
  )
}

generate_new_vctr_class(
  "class", 
  "double",
  attrs = list(
    list(NAME = "sum", DEFAULT = "0L", PTYPE = "double"),
    list(NAME = "mean", DEFAULT = "0L")
  ))


generate_type_rules <- function(class, ...){
  rules <- list(...)
  purrr::map(
    .x = rules,
    .f = ~ glue::glue_data(
      .x = append(list(CLASS = class), .x),
      "vec_type2.vctrs_{CLASS}.{TYPE} <- function(x, y, ...) {TO}()",
      "vec_type2.{TYPE}.vctrs_{CLASS} <- function(x, y, ...) {TO}()",
      .sep = "\n"
    )
  ) %>%
    glue::glue_collapse(sep = "\n\n")
}

generate_cast_rules <- function(class, ...){
  rules <- list(...)
  purrr::map(
    .x = rules,
    .f = ~ glue::glue_data(
      .x = append(list(CLASS = class), .x),
      "vec_cast.vctrs_{CLASS}.{TYPE} <- function(x, y, ...) {CLASS}()",
      "vec_cast.{TYPE}.vctrs_{CLASS} <- function(x, y, ...) {TO}()",
      .sep = "\n"
    )
  ) %>%
    glue::glue_collapse(sep = "\n\n")
}



generate_template_arglist <- function(class, 
                                      ptype,
                                      desc,
                                      formatters,
                                      attrs  = NULL,
                                      type_rules = NULL,
                                      cast_rules = NULL){
  
  list(
    # Name of the class
    CLASS      = class,
    
    # Class contructors
    CLASS_CONS = generate_new_vctr_class(
      class, 
      ptype,
      attrs = attrs),
    
    # Some descriptive text about the class
    CLASS_DESC = desc,
    
    # Prototype class
    PTYPE      = ptype,
    
    # Name of the formatting function (signature must be (x, ...))
    FORMATTERS  = generate_arg_string(formatters),
    
    ## Coercion rules
    COMMON_TYPES = do.call("generate_type_rules", 
                           args = append(list(class = class), type_rules)),
    
    ## Casting rules
    CASTING = do.call("generate_cast_rules",
                      args = append(list(class = class), cast_rules))
      # list(TYPE = "double",  TO = "vec_data"),
      # list(TYPE = "numeric", TO = "vec_data"))
  )
}
