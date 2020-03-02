#' Study species
#'
#' Get the species used in a study
#' @param study_id the id of a study e.g. "ST000001". Can be a substring to search by.
#' @import httr plyr
#' @return a data frame
#' @export
study_species = function(study_id) {

    input_item=list('study_id' = study_id)

    expected=data.frame(
        'Study ID'=NA,
        'Latin name'=NA,
        'Common name'=NA
    )

    out=workbench_get_study(input_item,'species',expected=expected)

    return(out)
}

