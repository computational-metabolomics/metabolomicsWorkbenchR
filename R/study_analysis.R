#' Study analysis
#'
#' Returns a number of key instrumentation parameters.
#' @import httr plyr
#' @return a list
#' @param keyword the keyword to search by. One of [study_id,study_title,last_name,institute]
#' @param value the keyword value. Can be a substring to search by.
#' @export
study_analysis = function(keyword,value) {

    if (length(keyword)>1) {
        stop('only one keyword allowed')
    }

    if (!(keyword %in% c('study_id','study_title','last_name','institute'))) {
        stop(paste0('"', keyword, '" is not a valid keyword for this search'))
    }

    input_item=list(value)
    names(input_item)=keyword

    expected= expected=data.frame(
        'study_id'=NA,
        'analysis_id'=NA,
        'analysis_summary'=NA,
        'analysis_type'=NA,
        'instrument_name'=NA,
        'instrument_type'=NA)

    out=workbench_get_study(input_item,'analysis',expected=expected)

    return(out)
}
