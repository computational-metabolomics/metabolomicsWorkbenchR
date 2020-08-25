#' Check validity of context and inputs/outputs
#'
#' Compares the input and output items to the expected values for a context and 
#' returns TRUE if the inputs/puts are valid for the provided context or an 
#' error if not. This method is used internally and not for intended for general
#' use.
#' @param context An mw_context object.
#' @param input_item An mw_input_item object, or the name of one.
#' @param input_value The value for the input item (character).
#' @param output_item An mw_output_item, or the name of one.
#' @return TRUE or an error.
#' @examples
#' is_valid(context$study,
#'   input_item$study_id$name,
#'   'ST000001',
#'   output_item$summary$name
#' )
#' @rdname is_valid
#' @export
setGeneric("is_valid",
    function(context,input_item,input_value,output_item)standardGeneric(
        "is_valid"))

#' Query the Metabolomics Workbench database
#'
#' Sends a query to the metabolomics database and returns the result. Note that
#' while objects derived from mw_base can be used the recommended approach is to
#' use character inputs.
#' @param context A valid context name (character)
#' @param input_item A valid input_item name (character)
#' @param input_value The value for the input item (character).
#' @param output_item A valid output_item (character).
#' @return A data.frame, or other output appropriate to the output_item.
#' @examples
#' # Get a summary of all studies with "diabetes" in the title
#' df = do_query(
#'   context = 'study',
#'   input_item = 'study_title',
#'   input_value = 'diabetes',
#'   output_item = 'summary'  
#' )
#' @rdname do_query
#' @export
setGeneric("do_query",
    function(context,input_item,input_value,output_item)standardGeneric(
        "do_query"))

#' Check input against acceptable input pattern
#'
#' Checks an input_value against a regex pattern to determine if the input_value
#' is valid. This method is used internally and not for intended for 
#' general use.
#' @param I An mw_input_item.
#' @param input_value The value for the input item (character).
#' @param match The type of match. One of "exact" or "partial".
#' @return TRUE if input matches the pattern, or throws an error.
#' @examples
#' check_pattern(input_item$study_id,'ST000001','exact')
#' @rdname check_pattern
#' @export
setGeneric("check_pattern",
    function(I,input_value,match)standardGeneric(
        "check_pattern"))

#' Check inputs/outputs match
#'
#' Checks that the provided inputs and output are compatible. This method 
#' is used internally and not for intended for general use.
#' @param input_item An mw_input_item.
#' @param output_item An mw_output_item.
#' @return TRUE if the items are compatible or throws an error if not.
#' @examples
#' check_puts(input_item$study_id,output_item$summary)
#' @rdname check_puts
#' @export
setGeneric("check_puts",
    function(input_item,output_item)standardGeneric(
        "check_puts"))
