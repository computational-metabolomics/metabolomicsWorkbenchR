#' Study number of metabolites
#'
#' Get the number of metabolites
#' @param keyword the keyword to search by. One of [study_id,study_title,last_name,institute]
#' @param value the keyword value. Can be a substring to search by.
#' @import httr plyr
#' @return a data frame
#' @export
study_number_metabolites = function(keyword,value) {

    if (length(keyword)>1) {
        stop('only one keyword allowed')
    }

    if (!(keyword %in% c('study_id','study_title','last_name','institute'))) {
        stop(paste0('"', keyword, '" is not a valid keyword for this search'))
    }

    input_item=list(value)
    names(input_item)=keyword

    expected=data.frame(
        'study_id'=NA,
        'analysis_id'=NA,
        'study_title'=NA,
        'num_metabolites'=NA,
        'analysis_display'=NA
    )

    out=workbench_get_study(input_item,'number_of_metabolites',expected=expected)

    return(out)
}

