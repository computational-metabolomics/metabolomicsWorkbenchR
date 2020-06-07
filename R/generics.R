#' @export
setGeneric("is_valid",function(context,input_item,input_value,output_item)standardGeneric("is_valid"))

#' @export
setGeneric("do_query",function(context,input_item,input_value,output_item)standardGeneric("do_query"))

#' @export
setGeneric("check_pattern",function(I,input_value,match)standardGeneric("check_pattern"))

#' @export
setGeneric("check_puts",function(input_item,output_item)standardGeneric("check_puts"))