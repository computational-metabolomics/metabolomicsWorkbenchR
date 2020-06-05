
#' @export
parse_data_frame=function(out,Q) {
    
    # force to list of lists in case of length = 1
    if (!is.list(out[[1]])) {
        out=list(out)
    }
    
    # create data.frame with expected return column names
    expected = as.data.frame(matrix(NA,nrow=0,ncol=length(Q$output_item$fields)))
    colnames(expected) = Q$output_item$fields
    # add it to the list
    out[[length(out)+1]]=expected
    
    # rbind the list and pad with NA if field not returned
    out = data.table::rbindlist(out,fill=TRUE)
    out = as.data.frame(out)
    # NB added row of NA already dropped
    
    # return the df
    return(out)
}

#' @export
parse_factors=function(out,Q) {
    
    # parse output to data.frame
    out_orig = parse_data_frame(out,Q)
    
    # create a list with factors for each sample_id
    u = unique(out_orig$study_id)
    OUT=list()
    
    for (id in u) {
        # subset study_id
        out=out_orig[out_orig$study_id==id,,drop=FALSE]
        
        # expand factors
        m=as.data.frame(matrix(NA,nrow=nrow(out),ncol=20)) # assume no more than 20 factors for a study
        
        for (k in seq(from=1,to=nrow(out))) {
            
            x=out$factors[k]
            
            # generate a data.frame from the factors by splitting at the pipes
            at_pipe=strsplit(x,'|',fixed=TRUE)[[1]]
            
            for (j in seq(from=1, to=length(at_pipe))) {
                # split each factor at colon
                at_colon=strsplit(at_pipe[[j]],':',fixed=TRUE)[[1]]
                
                m[k,j]=at_colon[2]
                
                fname=gsub('([[:punct:]])|\\s+','_',at_colon[1])
                if (substr(fname,1,1)=='_') {
                    fname=substr(fname,2,nchar(fname))
                }
                colnames(m)[j]=fname
            }
        }
        
        m=m[,seq(from=1,to=length(at_pipe)),drop=FALSE] # set strings to factors
        
        m=as.data.frame(lapply(m,factor))
        
        # bind the factors to the origin data.frame
        out=cbind(out,m)
        
        # remove superfluous info
        out=out[,-4]
        
        OUT[[id]]=out
    }
    return(OUT)
}