#' @export
setGeneric("is_valid",function(context,input_item,input_value,output_item)standardGeneric("is_valid"))

#' @export
setGeneric("do_query",function(Q)standardGeneric("do_query"))

#' @export
setGeneric("check_pattern",function(I,input_value,match)standardGeneric("check_pattern"))