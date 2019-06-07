generate_arg_string <- function(x){
  if(is.null(x)){
    ""
  } else {
    sprintf("c('%s'),", paste(x, collapse = "','"))
  }
}

to_function_string <- function(x){
  sprintf("%s()", x)
}

generate_type_rules <- function(class, ...){
  rules <- list(...)
  out <- purrr::map(
    .x = rules,
    .f = ~ glue::glue_data(
      .x = append(list(CLASS = class), .x),
      "vec_type2.vctrs_{CLASS}.{TYPE} <- function(x, y, ...) {TO}()",
      "vec_type2.{TYPE}.vctrs_{CLASS} <- function(x, y, ...) {TO}()",
      .sep = "\n"
    )
  )
  
  glue::glue_collapse(out, sep = "\n\n")
}

generate_cast_rules <- function(class, ...){
  rules <- list(...)
  
  out <- purrr::map(
    .x = rules,
    .f = ~ glue::glue_data(
      .x = append(list(CLASS = class), .x),
      "vec_cast.vctrs_{CLASS}.{TYPE} <- function(x, y, ...) {CLASS}()",
      "vec_cast.{TYPE}.vctrs_{CLASS} <- function(x, y, ...) {TO}()",
      .sep = "\n"
    )
  )
  
  glue::glue_collapse(out, sep = "\n\n")
}


generate_template_arglist <- function(class, 
                                      ptype,
                                      desc,
                                      formatters  = NULL,
                                      descriptors = NULL,
                                      type_rules  = NULL,
                                      cast_rules  = NULL){
  
  list(
    # Name of the class
    CLASS      = class,

    # Some descriptive text about the class
    CLASS_DESC = desc,
    
    # Prototype class
    PTYPE      = ptype,
    
    # Name of the descriptor functions
    DESCRIPTORS  = generate_arg_string(descriptors),
    
    # Name of the formatting functions (signature must be (x, ...))
    FORMATTERS  = generate_arg_string(formatters),
    
    ## Coercion rules
    COMMON_TYPES = do.call("generate_type_rules", 
                           args = append(list(class = class), type_rules)),
    
    ## Casting rules
    CASTING = do.call("generate_cast_rules",
                      args = append(list(class = class), cast_rules))
  )
}
