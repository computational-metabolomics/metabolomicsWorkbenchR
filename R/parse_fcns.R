
parse_data_frame=function(out,Q) {
    
    # force to list of lists in case of length = 1
    if (!is.list(out[[1]])) {
        out=list(out)
    }
    
    # tidy column names
    out=lapply(out,function(x){
        # lower case
        names(x)=tolower(names(x))
        # replace special with underscore
        names(x)=gsub('([[:punct:]])|\\s+','_',names(x))
        return(x)
    })
    
    
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

parse_untarg_factors=function(out,Q) {
    df = as.data.frame(unlist(out))
    # remove number
    df$group=trimws(gsub(pattern='^[0-9]+.',replacement='',x=rownames(df)))
    colnames(df)[1]='count'
    df$count=as.numeric(df$count)
    # create factor columns
    m = create_factor_columns(df,'group')
    # add the factors into back into the table
    w=which(colnames(df)=='group')
    # exclude group
    df=cbind(df[1:max(w-1,1)],m,df[min((w+1),ncol(df)):ncol(df)])
    rownames(df)=1:nrow(df)
    df$group=NULL
    return(df)
}

parse_data=function(out,Q) {
    
    out=lapply(out,function(x){
        x$DATA=lapply(x$DATA,function(y){
            if (is.null(y)) {
                y=NA
            }
            return(y)
        })
        return(x)
    })
    out=lapply(out,data.frame)
    out=data.table::rbindlist(out,fill=TRUE)
    colnames(out)=gsub('DATA.','',colnames(out),fixed=TRUE)
    # convert to numeric
    out[,8:ncol(out)]=lapply(out[,8:ncol(out)],as.numeric)
    return(out)
}

parse_untarg_data=function(out,Q) {
    
    # split the group column at the pipe to get factors
    m = create_factor_columns(out,'group')
    
    # add the factors into back into the table
    w=which(colnames(out)=='group')
    # exclude group
    out=cbind(out[1:(w-1)],m,out[(w+1):ncol(out)])
    return(out)
}

parse_datatable=function(out,Q) {
    
    # split the group column at the pipe to get factors
    m = create_factor_columns(out,'Class')
    
    # add the factors into back into the table
    w=which(colnames(out)=='Class')
    # exclude group
    out=cbind(out[1:(w-1)],m,out[(w+1):ncol(out)])
    return(out)
}

create_factor_columns = function(out,fn) {
    # create factor columns
    m=matrix(NA,nrow=nrow(out),ncol=20) # assume no more than 20 factors
    # split at pipe
    at_pipe = strsplit(out[[fn]],'|',fixed=TRUE)
    
    m=lapply(at_pipe,function(x){
        at_colon=strsplit(x,':',fixed=TRUE)
        m=matrix(NA,nrow=1,ncol=length(at_colon))
        n=matrix(interaction('V',1:length(at_colon),sep=''),nrow=1,ncol=ncol(m))
        for (j in 1:length(at_colon)) {
            m[1,j]=trimws(at_colon[[j]][length(at_colon[[j]])])
            if (length(at_colon[[j]])==2) {
                n[1,j]=trimws(at_colon[[j]][1])
            }
        }
        m=as.data.frame(m)
        colnames(m)=n
        return(m)
    })
    m=data.table::rbindlist(m,fill=TRUE)
    return(m)
}