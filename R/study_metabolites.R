#' Study metabolites
#'
#' Details for each named metabolite in a particular study or
#' analysis
#' @import httr plyr
#' @param id the study or analysis id e.g. "ST000001"
#' @return a list
#' @export
study_metabolites = function(id) {

    str=substr(id,start=1,stop=2)
    if (str=='ST')  {
        type='study_id'
    } else if (str=='AN') {
        type='analysis_id'
    } else {
        stop('ID type not recognised. Expected STxxxxxx or ANxxxxxx')
    }

    input_item=list(id)
    names(input_item)=type

    expected=data.frame(
        'study_id' = NA,
        'analysis_id' = NA,
        'analysis summary' = NA,
        'metabolite_name' = NA,
        'refmet_name' = NA,
        'pubchem_id' = NA,
        'other_id' = NA,
        'other_id_type' = NA
    )

    out=workbench_get_study(input_item,'metabolites',expected=expected)

    return(out)
}
