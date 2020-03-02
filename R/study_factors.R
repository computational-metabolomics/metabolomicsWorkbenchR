#' Study factors
#'
#' Get factors for a study
#' @param study_id the id of a study e.g. "ST000001". Must be a single study id.
#' @import httr plyr
#' @return factors for a study
#' @export
study_factors = function(study_id) {
    input_item=list('study_id' = study_id)

    expected=data.frame(
        'study_id'=NA,
        'local_sample_id'=NA,
        'subject_type'=NA,
        'factors'=NA)

    out=workbencheR:::workbench_get_study(input_item,'factors',expected=expected)


    m=as.data.frame(matrix(NA,nrow=nrow(out),ncol=20))

    for (k in seq(from=1,to=nrow(out))) {

        x=out$factors[k]

        # generate a data.frame from the factors by splitting at the pipes
        at_pipe=strsplit(x,'|',fixed=TRUE)[[1]]

        for (j in seq(from=1, to=length(at_pipe))) {
            # split each factor at colon
            at_colon=strsplit(at_pipe[[j]],':',fixed=TRUE)[[1]]

            m[k,j]=at_colon[2]
            colnames(m)[j]=at_colon[1]
        }
    }

    m=m[,seq(from=1,to=length(at_pipe))] # set strings to factors

    out=cbind(out,m)
    out=out[,-4]
    return(out)
}

