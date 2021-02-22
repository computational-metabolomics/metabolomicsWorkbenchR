
parse_data_frame=function(response,output_item,input_value) {
    out=httr::content(response,as='text',encoding = 'UTF-8')
    # parse json
    out=jsonlite::fromJSON(out)
    
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
    expected = as.data.frame(matrix(NA,nrow=0,ncol=length(output_item$fields)))
    colnames(expected) = output_item$fields
    # add it to the list
    out[[length(out)+1]]=expected
    
    # rbind the list and pad with NA if field not returned
    out = data.table::rbindlist(out,fill=TRUE)
    out = as.data.frame(out)
    # NB added row of NA already dropped
    
    # return the df
    return(out)
}

parse_factors=function(response,output_item,input_value) {
    
    # parse output to data.frame
    out_orig = parse_data_frame(response,output_item,input_value)
    
    # create a list with factors for each sample_id
    u = unique(out_orig$study_id)
    OUT=list()
    
    for (id in u) {
        # subset study_id
        out=out_orig[out_orig$study_id==id,,drop=FALSE]
        
        # expand factors
        # assume no more than 20 factors for a study
        m=as.data.frame(matrix(NA,nrow=nrow(out),ncol=20)) 
        
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

parse_untarg_factors=function(response,output_item,input_value) {
    out=httr::content(response,as='text',encoding = 'UTF-8')
    # parse json
    out=jsonlite::fromJSON(out)
    
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
    df=cbind(df[seq_len(max(w-1,1))],m,df[min((w+1),ncol(df)):ncol(df)])
    rownames(df)=seq_len(nrow(df))
    df$group=NULL
    return(df)
}

parse_data=function(response,output_item,input_value) {
    out=httr::content(response,as='text',encoding = 'UTF-8')
    # parse json
    out=jsonlite::fromJSON(out)
    
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
    
    # check for multiple analysis_id
    u=unique(out$analysis_id)
    
    # split by analysis_id
    out2=list()
    for (k in u) {
        out2[[k]]=out[out$analysis_id==k,]
    }
    
    return(out2)
}

parse_untarg_data=function(response,output_item,input_value) {
    out=httr::content(response,as='text',encoding = 'UTF-8')
    out=read.table(
        text=out,
        sep='\t',
        header = TRUE,
        row.names = 1,
        check.names = FALSE)
    
    # split the group column at the pipe to get factors
    m = create_factor_columns(out,'group')
    
    # add the factors into back into the table
    w=which(colnames(out)=='group')
    # convert to numeric
    temp=lapply(out[(w+1):ncol(out)],as.numeric)
    temp=as.data.frame(temp)
    rownames(temp)=rownames(out)
    colnames(temp)=colnames(out[(w+1):ncol(out)])
    # exclude group
    out=cbind(out[seq_len(w-1)],m,temp)
    
    out$group=NULL
    return(out)
}

parse_datatable=function(response,output_item,input_value) {
    
    out=httr::content(response,as='text',encoding = 'UTF-8')
    out=read.delim(
        text=out,
        sep='\t',
        row.names = 1,
        header = TRUE,
        check.names = FALSE)
    
    # split the group column at the pipe to get factors
    m = create_factor_columns(out,'Class')
    
    # add the factors into back into the table
    w=which(colnames(out)=='Class')
    # convert to numeric
    temp=lapply(out[(w+1):ncol(out)],as.numeric)
    temp=as.data.frame(temp)
    rownames(temp)=rownames(out)
    colnames(temp)=colnames(out[(w+1):ncol(out)])
    # exclude group
    out=cbind(out[seq_len(w-1)],m,temp)
    out$Class=NULL
    attributes(out)=c(attributes(out),list('number_of_factors'=ncol(m)))
    
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
        n=matrix(interaction('V',seq_along(at_colon),sep=''),
            nrow=1,
            ncol=ncol(m))
        for (j in seq_along(at_colon)) {
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

parse_do_nothing=function(response,output_item,input_value) {
    out=httr::content(response)
    return(out)
}

parse_moverz=function(response,output_item,input_value) {
    out=httr::content(response,as='text',encoding = 'UTF-8')
    
    if (input_value[[1]]=='MB') {
        # remove extra column of - - - 
        out2=gsub(pattern = '\t-\t-\t-\n',replacement = '\n',out)
        # remove extra column of - - 
        out2=gsub(pattern = '\t-\t-\n',replacement = '\n',out2)
        # replace - - - with -
        out2=gsub(pattern = '\t-\t-\t-\t',replacement = '\t-\t',out2)
    } else if (input_value[[1]]=='LIPIDS') {
        # remove extra column 
        out2=gsub(pattern = '\t\n',replacement = '\n',out)
        # split merged columns
        info=gregexpr('[0-9]+\\.[0-9]+\\.[0-9]+',out2)
        found=regmatches(out2,info)[[1]]
        replace=unlist(lapply(found,function(x){
            n=nchar(x)
            x=paste0(
                c(substr(x,1,n-5),paste0('0',substr(x,n-4,n))),
                collapse='\t')
        }))
        # for each match
        for (k in seq_along(found)) {
            out2=sub(pattern = found[k],replacement = replace[k],x=out2)
        }
    } else if (input_value[[1]]=='REFMET'){
        out2=out
    }
    
    # datatable
    out=read.delim(text=out2,sep='\t',na.strings = '-')
    colnames(out)=tolower(colnames(out))
    colnames(out)=gsub('\\.','_',colnames(out))
    colnames(out)=gsub('m_z','mz',colnames(out))
    return(out)
}

parse_exactmass=function(response,output_item,input_item) {
    out=httr::content(response,as='text',encoding = 'UTF-8')
    out2=gsub(pattern = '</br>',replacement = '',out)
    out2=gsub(pattern = '\n\n\n',replacement = '',out2)
    out2=gsub(pattern = '\n',replacement = '\t',out2)
    out=read.delim(text=out2,header=FALSE,sep='\t')
    colnames(out)=output_item$fields
    return(out)
}