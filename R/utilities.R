context_printer <- function(x){
  ctxt <- get_context(x)
  if(is_empty(ctxt)){ return("")}
  prps <- methods::slot(get_context(x), "purpose")
  prps <- if(length(prps) == 0 || prps == "") "<undefined>" else prps
  sprintf("Purpose: %s\n", prps)
}

desc_printer <- function(x, label, what){
  sprintf("%s: %s", label, attr(x, "desc")[[what]])
}