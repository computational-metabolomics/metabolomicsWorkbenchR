.onLoad <-
    function(libname = find.package('workbencheR'),
        pkgname = 'workbencherR')
    {
        options('BASE_URL' = 'https://www.metabolomicsworkbench.org/rest/')

    }


workbench_get_study=function(input_item,output_item,output_format=NULL,expected=NULL) {

    # build url
    context='study'
    str=paste0(getOption('BASE_URL'),context)

    for (k in seq.int(from=1,to=length(input_item))) {
        str=paste(str,names(input_item)[k],input_item[k],sep = '/')
    }

    str=paste(str,output_item,sep = '/')

    if (output_item %in% c('datatable','untarg_data')) {
        out = httr::GET(
            url=str,
            httr::add_headers(
                Accept ='application/json',
                `Content-Type`='application/json')
        )


    } else {
        out = httr::GET(
            url=str
        )

        out=httr::content(out,'parsed')

        if (length(out)==0){
            return(expected)
        }
        # force to list of lists
        if (!is.list(out[[1]])) {
            out=list(out)
        }

        # check for null and replace with NA is all DATA lists
        if (output_item=='data') {
            out=lapply(out,function(x) {
                d=x$DATA
                d=lapply(d,function(y){
                    if(is.null(y)){
                        y=NA
                    }
                    return(y)
                })
                x$DATA=d
                return(x)
            })
        }

        # convert to list of data.frames
        out=lapply(out,as.data.frame,stringsAsFactors=FALSE)
        # add expected to ensure all columns always returned
        if (!is.null(expected)) {
            out[[length(out)+1]]=expected
        }
        # collapse to data.frame and pad with NA where missing
        out=do.call(plyr::rbind.fill,out)
        # removed extra expected row
        if (!is.null(expected)) {
            out=out[-nrow(out),]
        }
    }
    return(out)
}
