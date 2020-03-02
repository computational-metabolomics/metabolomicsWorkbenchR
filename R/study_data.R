#' Study data
#'
#' Get the measured values for each metabolite in a study
#' @param id the study or analysis id of a study e.g. "ST000001".
#' @import httr plyr
#' @return a data frame
#' @export
study_data = function(id) {

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
        "study_id"=NA,
        "analysis_id"=NA,
        "analysis_summary"=NA,
        "metabolite_name"=NA,
        "metabolite_id"=NA,
        "refmet_name"=NA,
        "units"=NA
    )
    out=workbench_get_study(input_item,'data',expected=expected)

    # remove DATA from colnames
    cn=colnames(out)
    cn=lapply(cn,function(x) {
        if (grepl('DATA.',x)) {
            x=substr(x,start=6,stop=nchar(x))
        }
        return(x)
    })
    colnames(out)=cn


    return(out)
}

