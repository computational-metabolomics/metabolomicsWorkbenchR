#' Study source
#'
#' Get the source (blood, urine, liver etc) of a study
#' @param study_id the id of a study e.g. "ST000001". Can be a substring to search by.
#' @import httr plyr
#' @return a data frame
#' @export
study_source = function(study_id) {

    input_item=list('study_id' = study_id)

    expected=data.frame(
        'Study ID'=NA,
        'Sample source'=NA
        )

    out=workbench_get_study(input_item,'source',expected=expected)

    return(out)
}

