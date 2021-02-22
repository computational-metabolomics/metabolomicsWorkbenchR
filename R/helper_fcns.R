#' Valid inputs
#' 
#' Get a list of valid input_items for a context.
#' 
#' @param context The name of a valid context (character)
#' @return A list of input item names for a context
#' @examples
#' # list of input items for the "study" context
#' context_inputs("study")
#' @export
context_inputs = function(context) {
    if (!is.character(context)) {
        stop('"context" must be of type "character"')
    }
    
    name_valid = context %in% names(metabolomicsWorkbenchR::context)
    if (!name_valid) {
        stop(paste0('"',context,'" is not a recognised context. Use ',
            'names(context) for a list of valid contexts'))
    }
    
    return(metabolomicsWorkbenchR::context[[context]]$input_items)
}

#' Valid outputs
#' 
#' Get a list of valid output_items for a context.
#' 
#' @param context The name of a valid context (character)
#' @return A list of output item names for a context
#' @examples
#' # list of output items for the "study" context
#' context_outputs("study")
#' @export
context_outputs = function(context) {
    if (!is.character(context)) {
        stop('"context" must be of type "character"')
    }
    
    name_valid = context %in% names(metabolomicsWorkbenchR::context)
    if (!name_valid) {
        stop(paste0('"',context,'" is not a recognised context. Use ',
            'names(context) for a list of valid contexts'))
    }
    
    return(metabolomicsWorkbenchR::context[[context]]$output_items)
}

#' Valid input_value for input_item
#' 
#' Displays a valid input_value for an input_item and returns an example that
#' matches the required input pattern.
#' 
#' @param input_item The name of a valid input_item (character)
#' @return An example input value matching the pattern required for the chosen
#' input item.
#' @examples
#' # example input_value for input item "study_id" 
#' input_example('study_id')
#' @export
input_example = function(input_item) {
    if (!is.character(input_item)) {
        stop('"input_item" must be of type "character"')
    }
    
    name_valid = input_item %in% names(metabolomicsWorkbenchR::input_item)
    if (!name_valid) {
        stop(paste0('"',input_item,'" is not a recognised input_item. Use ',
            'names(input_item) for a list of valid input items'))
    }
    
    show(metabolomicsWorkbenchR::input_item[[input_item]])
    
    return(
        invisible(metabolomicsWorkbenchR::input_item[[input_item]]$example[1]))
}


