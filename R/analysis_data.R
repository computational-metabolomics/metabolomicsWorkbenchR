#' Study datatable
#'
#' Get the measured metabolite values for an analysis (NB may have been
#' normalised etc see analysis for details). Does not return class labels as per
#' api documentation; use `study_factors`
#' @param analysis_id the id of a study e.g. "AN000001".
#' @import httr plyr
#' @return a data frame
#' @export
analysis_data = function(analysis_id,untarg=FALSE) {

    input_item=list('analysis_id' = analysis_id)

    if (!untarg) {
        output_item='datatable'
    } else {
        output_item='untarg_data'
    }

    out=workbench_get_study(input_item,output_item)

    out=httr::content(out,as='text')

    out=read.table(text = out,sep='\t',stringsAsFactors = FALSE)
    colnames(out)=out[1,]
    out=out[-1,]

    out[,3:ncol(out)]=as.data.frame(lapply(out[,3:ncol(out)],as.numeric))
    out[,2]=factor(out$Class)

    rownames(out)=out$Samples
    out=out[,-c(1,2)]

    return(out)
}

