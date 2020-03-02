#' Study disease
#'
#' Get the disease of a study
#' @param study_id the id of a study e.g. "ST000001". Can be a substring to search by.
#' @import httr plyr
#' @return a data frame
#' @export
study_disease = function(study_id) {

    input_item=list('study_id' = study_id)

    expected=data.frame(
        'Study ID'=NA,
        'Disease'=NA
    )

    out=workbench_get_study(input_item,'disease',expected=expected)

    return(out)
}

