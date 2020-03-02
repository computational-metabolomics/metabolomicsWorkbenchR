#' Untargeted studies
#'
#' Summary information for all untargeted studies
#' @import httr plyr
#' @return a data frame
#' @export
study_untarg = function() {

    input_item=list('study_id'='X')

    expected=data.frame(
        'study_id'=NA,
        'analysis_id'=NA,
        'analysis_display'=NA,
        'study_title'=NA,
        'subject_species'=NA,
        'institute'=NA
    )
    out=workbench_get_study(input_item,'untarg_studies',expected=expected)

    return(out)
}

