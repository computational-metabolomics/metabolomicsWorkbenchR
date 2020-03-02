#' Study summary
#'
#' Get a study summaries
#' @param study_id the id of a study e.g. "ST000001". Can be a substring e.g.
#' "ST" or "ST0004" will return summaries for all studies matching the substring.
#' @import httr plyr
#' @return a list of studies and summary information
#' @export
study_summary = function(study_id) {

    input_item=list('study_id' = study_id)

    expected=data.frame(
        'study_id'=NA,
        'study_title'=NA,
        'study_type'=NA,
        'institute'=NA,
        'department'=NA,
        'last_name'=NA,
        'first_name'=NA,
        'email'=NA,
        'phone'=NA,
        'submit_date'=NA,
        'study_summary'=NA,
        'subject_species'=NA)

    out=workbench_get_study(input_item,'summary',expected=expected)

    return(out)
}

