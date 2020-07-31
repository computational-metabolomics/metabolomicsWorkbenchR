#' @include generics.R parse_fcns.R
#' @import methods
#' @import utils

############################## BASE ##########################################
mw_base = function(private,locked){
    out = new('mw_base',
        private = private,
        locked = locked
    )
    return(out)
}

.mw_base = setClass(
    Class = 'mw_base',
    slots = c(
        private = 'character',
        locked = 'character'
    )
)

#' Get slot value from mw_base objects
#' 
#' Gets the value of a slot from mw_base objects, provided they are not listed as
#' 'private'.
#' @param x An object derived from mw_base.
#' @param name The name of the slot to access.
#' @return The assigned to the slot.
#' @examples
#' # an object derived from mw_base object
#' C = context$study
#' # access the name slot
#' C$name
#' @rdname mw_base
#' @export
setMethod(f = "$",
    signature = c("mw_base"),
    definition = function(x,name) {
        
        is_slot = name %in% slotNames(x)
        is_private = name %in% c(x@private,'private')
        
        if (is_slot & !is_private) {
            return(slot(x,name))
        } else {
            if (!is_slot) {
                stop(paste0('"',name,'" is not a valid slot for objects of class "', class(x)[1],'"'))
            }
            if (is_private) {
                stop(paste0('"',name,'" is a private slot for internal use only.'))
            }
        }
    }
)


############################## CONTEXTS ######################################

mw_context = function(name,input_items,output_items,...) {
    out = new('mw_context',
        name = name,
        input_items = input_items,
        output_items = output_items,
        locked = c('name','input_items','output_items'),
        ...
    )    
    return(out)
}

.mw_context = setClass(
    Class = 'mw_context',
    contains = 'mw_base',
    slots = c(
        name = 'character',
        input_items = 'character',
        output_items = 'character'
    )
)


setMethod(f = 'show',
    signature = 'mw_context',
    definition = function(object) {
        cat('A Metabolomics Workbench "context"\n\n')
        cat('Name:\t"',object@name,'"\n\n',sep='')
        cat('Valid input_item names:\n')
        cat(paste0('\t"',object@input_items,'"',collapse='\n'),'\n\n',sep='')
        cat('Valid output_item names:\n')
        cat(paste0('\t"',object@output_items,'"',collapse='\n'),'\n\n',sep='')
    }
)


#' @rdname is_valid
#' @export
setMethod(f = 'is_valid',
    signature = c('mw_context','character','character','character'),
    definition = function(context,input_item,input_value,output_item) {
        
        name_valid = context@name %in% c('study','compound','refmet','gene','protein','moverz','exactmass')
        input_valid = all(input_item %in% context@input_items)
        output_valid = all(output_item %in% context@output_items)
        length_valid = (length(input_value)==length(input_item))
        length_out_valid = !(length(input_item)>1)
        
        err=list()
        if (!name_valid) {
            err=c(err,paste0('name = "',input_item,'" is not a valid context name.\n'))
        }
        if (!input_valid) {
            err=c(err,paste0('An input_item is not valid for this context.\n'))
        }
        
        if (!output_valid) {
            err=c(err,paste0('An output_item is not valid for this context.\n'))
        }
        
        if (!length_valid) {
            err=c(err,"Length of input_value must be the same as length of input_item.\n")
        }
        if (!length_out_valid) {
            err=c(err,"Length of intput_item is limited to 1 for this context.\n")
        }
        
        if (length(err)>0) {
            stop(err)
        } else {
            return(TRUE)
        }
    })


mw_moverz_context = function(input_items,ion_types,tol_range,mz_range,...) {
    out = new('mw_moverz_context',
        name = 'moverz',
        input_items = input_items,
        output_items = 'moverz',
        ion_types = ion_types,
        tol_range = tol_range,
        mz_range = mz_range,
        locked = c('name','input_items','output_items','ion_types','tol_range','mz_range'),
        ...
    )    
    return(out)
}

.mw_moverz_context = setClass(
    Class = 'mw_moverz_context',
    contains = 'mw_context',
    slots = c(
        ion_types = 'character',
        tol_range = 'numeric',
        mz_range = 'numeric'
    )
)


#' @rdname is_valid
#' @export
setMethod(f = 'is_valid',
    signature = c('mw_moverz_context','character','character','missing'),
    definition = function(context,input_item,input_value) {
        
        input_valid = all(input_item %in% context@input_items)
        
        range_valid2 = as.numeric(input_value[2]) >= context@mz_range[1] & as.numeric(input_value[2]) <= context@mz_range[2]
        ion_valid = input_value[3] %in% context@ion_types
        range_valid4 = as.numeric(input_value[4]) >= context@tol_range[1] & as.numeric(input_value[4]) <= context@tol_range[2]
        database_valid=input_value[1] %in% c('LIPIDS','MB','REFMET')
        
        err=list()
        if (!input_valid) {
            err=c(err,paste0('An input_item is not valid for this context.\n'))
        }
        if (!range_valid2) {
            err=c(err,"input_value[2] is out of range for this context.\n")
        }
        if (!range_valid4) {
            err=c(err,"input_value[4] is out of range for this context.\n")
        }
        if (!ion_valid) {
            err=c(err,paste0('"',input_value[3], '" is not a valid ion for this context.\n'))
        }
        if (!database_valid) {
            err=c(err,paste0('"',input_value[1], '" is not a valid database for this context.\n'))
        }
        if (length(err)>0) {
            stop(err)
        } else {
            return(TRUE)
        }
    })

mw_exactmass_context = function(ion_types,lipid_types,...) {
    out = new('mw_exactmass_context',
        name = 'exactmass',
        input_items = c('lipid','ion'),
        output_items = 'exactmass',
        ion_types = ion_types,
        lipid_types = lipid_types,
        locked = c('name','input_items','output_items','ion_types','lipid_types'),
        ...
    )    
    return(out)
}

.mw_exactmass_context = setClass(
    Class = 'mw_exactmass_context',
    contains = 'mw_context',
    slots = c(
        ion_types = 'character',
        lipid_types = 'character'
    )
)



#' @rdname is_valid
#' @export
setMethod(f = 'is_valid',
    signature = c('mw_exactmass_context','character','character','missing'),
    definition = function(context,input_item,input_value) {
        
        str=strsplit(input_value[1],'(',fixed=TRUE)[[1]]
        lipid_valid = str[1] %in% context@lipid_types
        ion_valid = input_value[2] %in% context@ion_types
        
        err=list()
        if (!ion_valid) {
            err=c(err,paste0('"',input_value[2], '" is not a valid ion for this context.\n'))
        }
        if (!lipid_valid) {
            err=c(err,paste0('"',str[1], '" is not a valid Lipid for this context.\n'))
        }
        if (length(err)>0) {
            stop(err)
        } else {
            return(TRUE)
        }
    })



################# INPUT ITEMS ####################

mw_input_item = function(name,pattern,example='',...) {
    out = new('mw_input_item',
        name = name,
        pattern = pattern,
        example = example,
        locked = c('name','pattern')
    )
}

.mw_input_item = setClass(
    Class = 'mw_input_item',
    contains = 'mw_base',
    slots = c(name = 'character',
        pattern = 'list',
        example = 'character'
    )
)

setMethod(f = 'show',
    signature = 'mw_input_item',
    definition = function(object) {
        cat('A Metabolomics Workbench "input_item"\n\n')
        cat('Name:\t"',object@name,'"\n\n',sep='')
        cat('Exact pattern matching:\n')
        cat(paste0('\t"',object@pattern$exact,'"',collapse='\n'),'\n\n',sep='')
        cat('Partial pattern matching:\n')
        cat(paste0('\t"',object@pattern$partial,'"',collapse='\n'),'\n\n',sep='')
        
        if (any(object$example != '')) {
            cat('Examples: \n')
            cat(paste0('\t"',object@example,'"',collapse='\n'),'\n\n',sep='')
        }
    }
)

############### OUTPUT ITEMS #####################
mw_output_item = function(name,fields,inputs,parse_fcn,match) {
    out=new('mw_output_item',
        name=name,
        fields=fields,
        inputs=inputs,
        parse_fcn=parse_fcn,
        match=match,
        locked = c('name','fields','inputs','parse_fcn','match')
    )
    return(out)
}

.mw_output_item = setClass(
    Class = 'mw_output_item',
    contains = 'mw_base',
    slots = c(name = 'character',
        fields = 'character',
        inputs = 'character',
        parse_fcn = 'function',
        match = 'character'
    )
)

setMethod(f = 'show',
    signature = 'mw_output_item',
    definition = function(object) {
        cat('A Metabolomics Workbench "output_item"\n\n')
        cat('Name:\t"',object@name,'"\n\n',sep='')
        cat('Returns:\n')
        cat(paste0('\t"',object@fields,'"',collapse='\n'),'\n\n',sep='')
        cat('Allowed input_item names:\n')
        cat(paste0('\t"',object@inputs,'"',collapse='\n'),'\n\n',sep='')
        cat('Type of matching supported:')
        cat(paste0('\t"',object@match,'"',collapse='\n'),'\n\n',sep='')
    }
)

### do_query methods ###

#' @rdname do_query
#' @export
#' @importFrom data.table rbindlist
#' @import httr
#' @import jsonlite
setMethod(f = 'do_query',
    signature = c('character','character','character','character'),
    definition = function(context,input_item,input_value,output_item) {
        
        
        if (length(output_item)>1) {
            stop('output_item must be of length 1')
        }
        if (!(context %in% names(metabolomicsWorkbenchR::context))) {
            stop(paste0('"',context, '" is not a valid context.'))
        }
        if (!(all(input_item %in% names(metabolomicsWorkbenchR::input_item)))) {
            stop(paste0('An input_item is not valid.'))
        }
        if (!(all(output_item %in% names(metabolomicsWorkbenchR::output_item)))) {
            stop(paste0('An output_item is not valid.'))
        }
        
        # convert to objects
        context=metabolomicsWorkbenchR::context[[context]]
        output_item=metabolomicsWorkbenchR::output_item[[output_item]]
        
        if (length(input_item)>1) {
            input_item=as.list(input_item)
            for (k in 1:length(input_item)) {
                input_item[[k]]=metabolomicsWorkbenchR::input_item[[k]]
            }
        } else {
            input_item=metabolomicsWorkbenchR::input_item[[input_item]]
        }
        
        # query the database
        out=do_query(context,input_item,input_value,output_item)
        
        return(out)
    }
)

#' @rdname do_query
#' @importFrom data.table rbindlist
#' @import httr
#' @import jsonlite
setMethod(f = 'do_query',
    signature = c('mw_moverz_context','list','character','mw_output_item'),
    definition = function(context,input_item,input_value,output_item) {
        
        
        if (length(input_item)!=4) {
            stop('You must provide 4 input_item for the moverz context.')
        } 
        
        if (length(input_value)!=4) {
            stop('You must provide 4 input_value for the moverz context.')
        } 
        
        # get input list as strings
        namez = lapply(input_item,function(x) {
            if (is(x,'mw_input_item')) {
                x=x$name
                return(x)
            } else if (is(x,'character')) {
                return(x)
            } else {
                stop('input_item list must only contain characters or mw_input_item objects.')
            }
        })
        namez=unlist(namez)
        
        # check namez
        if (namez[1]!='database') {
            stop('input_item[1] must be "database"')
        }
        if (namez[2]!='mz') {
            stop('input_item[2] must be "mz"')
        }
        if (namez[3]!='ion') {
            stop('input_item[3] must be "ion"')
        }
        if (namez[4]!='tolerance') {
            stop('input_item[4] must be "tolerance"')
        }
        
        # check context
        context_valid = is_valid(
            context,
            namez,
            input_value
        )
        
        # build the url
        str=paste('https://www.metabolomicsworkbench.org/rest',
            context@name,
            paste(input_value,collapse='/',sep=''),
            sep='/')
        
        if (identical(Sys.getenv("TESTTHAT"), "true")) { 
            print(str) 
        }
        
        out = use_api(str,output_item,input_value)
        
        return(out)
        
    }
)

#' @rdname do_query
#' @export
setMethod(f = 'do_query',
    signature = c('mw_moverz_context','list','character','character'),
    definition = function(context,input_item,input_value,output_item) {
        if (output_item != 'moverz'){
            warning('output_item changed to "moverz"')
        }
        output_item=metabolomicsWorkbenchR::output_item$moverz
        out = do_query(context,input_item,input_value,output_item)
        return(out)
    }
)

#' @rdname do_query
#' @export
setMethod(f = 'do_query',
    signature = c('mw_moverz_context','list','character','missing'),
    definition = function(context,input_item,input_value,output_item) {
        output_item=metabolomicsWorkbenchR::output_item$moverz
        out = do_query(context,input_item,input_value,output_item)
        return(out)
    }
)

#' @rdname do_query
#' @export
setMethod(f = 'do_query',
    signature = c('mw_moverz_context','character','character','missing'),
    definition = function(context,input_item,input_value,output_item) {
        output_item=metabolomicsWorkbenchR::output_item$moverz
        input_item=metabolomicsWorkbenchR::input_item$moverz
        out = do_query(context,input_item,input_value,output_item)
        return(out)
    }
)

#' @rdname do_query
#' @export
setMethod(f = 'do_query',
    signature = c('character','character','character','missing'),
    definition = function(context,input_item,input_value,output_item) {
        if (context!='moverz') {
            stop('output_item must be specific if not a "moverz" context')
        }
        context=metabolomicsWorkbenchR::context[[context]]
        out = do_query(context,input_item,input_value)
        return(out)
    }
)


#' @rdname do_query
#' @export
#' @importFrom data.table rbindlist
#' @import httr
#' @import jsonlite
setMethod(f = 'do_query',
    signature = c('mw_exactmass_context','list','character','mw_output_item'),
    definition = function(context,input_item,input_value,output_item) {
        
        
        if (length(input_item)!=2) {
            stop('You must provide 2 input_item for the exactmass context.')
        } 
        
        if (length(input_value)!=2) {
            stop('You must provide 2 input_value for the exactmass context.')
        } 
        
        # get input list as strings
        namez = lapply(input_item,function(x) {
            if (is(x,'mw_input_item')) {
                x=x$name
                return(x)
            } else if (is(x,'character')) {
                return(x)
            } else {
                stop('input_item list must only contain characters or mw_input_item objects.')
            }
        })
        namez=unlist(namez)
        
        # check namez
        if (namez[1]!='lipid') {
            stop('input_item[1] must be "lipid"')
        }
        if (namez[2]!='ion') {
            stop('input_item[2] must be "ion"')
        }
        
        # check context
        context_valid = is_valid(
            context,
            namez,
            input_value
        )
        
        # build the url
        str=paste('https://www.metabolomicsworkbench.org/rest',
            context@name,
            paste(input_value,collapse='/',sep=''),
            sep='/')
        
        if (identical(Sys.getenv("TESTTHAT"), "true")) {
            print(str)
        }
        
        out = use_api(str,output_item,input_value)
        
        return(out)
        
    }
)

#' @rdname do_query
#' @export
setMethod(f = 'do_query',
    signature = c('mw_exactmass_context','list','character','character'),
    definition = function(context,input_item,input_value,output_item) {
        if (output_item != 'exactmass'){
            warning('output_item changed to "exactmass"')
        }
        output_item=metabolomicsWorkbenchR::output_item$exactmass
        out = do_query(context,input_item,input_value,output_item)
        return(out)
    }
)

#' @rdname do_query
#' @export
setMethod(f = 'do_query',
    signature = c('mw_exactmass_context','list','character','missing'),
    definition = function(context,input_item,input_value,output_item) {
        output_item=metabolomicsWorkbenchR::output_item$exactmass
        out = do_query(context,input_item,input_value,output_item)
        return(out)
    }
)

#' @rdname do_query
#' @export
setMethod(f = 'do_query',
    signature = c('mw_exactmass_context','character','character','missing'),
    definition = function(context,input_item,input_value,output_item) {
        output_item=metabolomicsWorkbenchR::output_item$exactmass
        input_item=metabolomicsWorkbenchR::input_item$exactmass
        out = do_query(context,input_item,input_value,output_item)
        return(out)
    }
)

#' @rdname do_query
#' @export
setMethod(f = 'do_query',
    signature = c('character','character','character','missing'),
    definition = function(context,input_item,input_value,output_item) {
        if (!(context %in% c('moverz','exactmass'))) {
            stop('output_item must be specified for this context')
        }
        context=metabolomicsWorkbenchR::context[[context]]
        out = do_query(context,input_item,input_value)
        return(out)
    }
)


#' @rdname do_query
#' @export
#' @importFrom data.table rbindlist
#' @import httr
#' @import jsonlite
setMethod(f = 'do_query',
    signature = c('mw_context','mw_input_item','character','mw_output_item'),
    definition = function(context,input_item,input_value,output_item) {
        
        # check the context is valid
        is_valid(context,input_item$name,input_value,output_item$name)
        # check the input pattern
        check_pattern(input_item,input_value,output_item$match)
        # check the output_item and input_item are compatible
        check_puts(input_item,output_item)
        
        # build the url
        str=paste('https://www.metabolomicsworkbench.org/rest',
            context@name,
            input_item$name,
            paste(input_value,collapse='/',sep=''),
            paste(output_item$name,collapse=',',sep=''),
            sep='/')
        
        if (identical(Sys.getenv("TESTTHAT"), "true")) {
            print(str)
        }
        out = use_api(str,output_item,input_value)
        
        return(out)
    }
)

#' @rdname check_pattern
#' @export
setMethod(f = 'check_pattern',
    signature = 'mw_input_item',
    definition = function(I,input_value,match) {
        pattern = I$pattern[[match]]
        valid = grepl(x=input_value,pattern=pattern)
        if (!valid) {
            stop(paste0('The input_value does not match the required pattern for the provided input_item and output_item.'))
        } else {
            return(TRUE)
        }
    }
)

#' @rdname check_puts
#' @export
setMethod(f = 'check_puts',
    signature = 'mw_input_item',
    definition = function(input_item,output_item) {
        valid = input_item$name %in% output_item$inputs
        if (!valid) {
            stop(paste0('The input_value is not compatible with the output_item.'))
        } else {
            return(TRUE)
        }
    }
)

use_api = function(str,output_item=NULL,input_value=NULL,testing=0) {
    
    if (identical(Sys.getenv("TESTTHAT"), "true")) {
        # return a stored result for testing
        print('TEST mode')
        response=R[[S[[output_item$name]]]]
    } else {
        # get a response from the API
        response = httr::GET(
            url=str
        )
    }
    
    
    if (response$headers$`content-type`=="image/png") {
        # do nothing
    } else {
        out=httr::content(response,as='text',encoding = 'UTF-8')
        if (out=='[]') {
            message('There were no results for your query.')
            return(NULL)
        }
    }
    
    out = output_item$parse_fcn(response,output_item,input_value)
    
    return(out)
}


##### SummarizedExperiment
mw_SE_item = function(name,fields,inputs,parse_fcn,match,...) {
    out=new('mw_SE_item',
        name=name,
        fields=fields,
        inputs=inputs,
        parse_fcn=parse_fcn,
        match=match,
        locked = c('name','fields','inputs','parse_fcn','match')
    )
    return(out)
}

.mw_SE_item = setClass(
    Class = 'mw_SE_item',
    contains = 'mw_output_item'
)

#' @rdname do_query
#' @export
#' @importFrom data.table rbindlist
#' @import httr
#' @import jsonlite
#' @import MultiAssayExperiment
setMethod(f = 'do_query',
    signature = c('mw_context','mw_input_item','character','mw_SE_item'),
    definition = function(context,input_item,input_value,output_item) {
        
        err=list()
        # check we have a study context
        if (context$name != 'study') {
            err=c(err,'SummarizedExperiment output_item can only be used with "study" context')
        }
        if(length(err)>0) {
            stop(err)
        }
        
        ## get data
        if (input_item$name == 'study_id') {
            df = do_query(context$name,input_item$name,input_value,'data')
            
            
            
            ## multiple analysis might be returned
            out=list()
            for (k in 1:length(df)) {
                
                S=do_query('study','study_id',input_value,'summary')
                
                # raw data
                X = df[[k]][,8:ncol(df[[k]])]
                rownames(X)=df[[k]]$metabolite_id
                
                # feature meta data
                VM  = df[[k]][,4:6]
                rownames(VM)=df[[k]]$metabolite_id
                
                # additional metadata
                M = list(
                    data_source = 'Metabolomics Workbench',
                    study_id=df[[k]]$study_id[1],
                    analysis_id=df[[k]]$analysis_id[1],
                    analysis_summary=df[[k]]$analysis_summary[1],
                    units=df[[k]]$units[1],
                    name=paste0(S$study_id[[1]],':',df[[k]]$analysis_id[1]),
                    description=S$study_title[[1]]
                )
                
                
                # factors
                SM = do_query('study','study_id',input_value,'factors')[[1]]
                # merge with data samples in case some are missing
                SM=merge(data.frame('local_sample_id'=colnames(X)),SM,by='local_sample_id',all=TRUE,sort=FALSE)
                rownames(SM)=SM$local_sample_id
                
                M[['subject_type']]=SM$subject_type[1]
                SM$subject_type=NULL
                
                # SE object
                SE = SummarizedExperiment(
                    assays=list(X),
                    rowData = VM,
                    colData = SM,
                    metadata = M
                )
                
                out[[df[[k]]$analysis_id[1]]]=SE
            }
            
            if (length(out)==1)  {
                out=out[[1]]
            }
            
            return(out)
        } else if (input_item$name == 'analysis_id') {
            df = do_query(context$name,input_item$name,input_value,'datatable')
            nf=attributes(df)$number_of_factors
            
            X=as.data.frame(t(df))
            
            SM=as.data.frame(t(X[1:nf,]))
            X=X[nf+1:nrow(X),]
            
            VM=data.frame(metabolite=rownames(X))
            
            rownames(X)=1:nrow(X)
            
            M = list(
                'data_source' = 'Metabolomics Workbench',
                'analysis_id' = input_value,
                'name'=input_value,
                'description'='Downloaded from Metabolomics Workbench'
            )
            
            SE = SummarizedExperiment(
                assays = X,
                rowData = VM,
                colData = SM,
                metadata = M
            )
            
            out=SE
            return(out)
        }
        
    }
)

##### SummarizedExperiment untargeted
mw_untarg_SE_item = function(name,fields,inputs,parse_fcn,match,...) {
    out=new('mw_untarg_SE_item',
        name=name,
        fields=fields,
        inputs=inputs,
        parse_fcn=parse_fcn,
        match=match,
        locked = c('name','fields','inputs','parse_fcn','match')
    )
    return(out)
}

.mw_untarg_SE_item = setClass(
    Class = 'mw_untarg_SE_item',
    contains = 'mw_output_item'
)

#' @rdname do_query
#' @export
#' @importFrom data.table rbindlist
#' @import httr
#' @import jsonlite
#' @import SummarizedExperiment
setMethod(f = 'do_query',
    signature = c('mw_context','mw_input_item','character','mw_untarg_SE_item'),
    definition = function(context,input_item,input_value,output_item) {
        
        ## get data
        
        df = do_query(context$name,input_item$name,input_value,'untarg_data')
        fq = do_query('study','analysis_id',input_value,'untarg_factors')
        
        X=as.data.frame(t(df))
        
        nf=ncol(fq)-1
        
        SM=as.data.frame(t(X[1:nf,]))
        X=X[nf+1:nrow(X),]
        
        VM=data.frame(feature_id=rownames(X))
        
        rownames(X)=1:nrow(X)
        
        M = list(
            'data_source' = 'Metabolomics Workbench (untargeted)',
            'analysis_id' = input_value
        )
        
        SE = SummarizedExperiment(
            assays = X,
            rowData = VM,
            colData = SM,
            metadata = M
        )
        
        return(SE)
        
    }
)


##### DatasetExperiment
mw_DE_item = function(name,fields,inputs,parse_fcn,match,...) {
    out=new('mw_DE_item',
        name=name,
        fields=fields,
        inputs=inputs,
        parse_fcn=parse_fcn,
        match=match,
        locked = c('name','fields','inputs','parse_fcn','match')
    )
    return(out)
}

.mw_DE_item = setClass(
    Class = 'mw_DE_item',
    contains = 'mw_output_item'
)

#' @rdname do_query
#' @export
#' @importFrom data.table rbindlist
#' @import httr
#' @import jsonlite
#' @import struct
setMethod(f = 'do_query',
    signature = c('mw_context','mw_input_item','character','mw_DE_item'),
    definition = function(context,input_item,input_value,output_item) {
        err=list()
        # check we have a study context
        if (context$name != 'study') {
            err=c(err,'DatasetExperiment output_item can only be used with "study" context')
        }
        if(length(err)>0) {
            stop(err)
        }
        
        # use SE, then convert to DE
        SE = do_query(context$name,input_item$name,input_value,'SummarizedExperiment')
        
        if (is(SE,'SummarizedExperiment')) {
            DE=as.DatasetExperiment(SE)
            DE$name=input_value
            if (input_item$name=='study_id') {
                desc=do_query('study',input_item$name,input_value,'summary')
                DE$description=desc$study_title[[1]]
            } else {
                DE$description = 'Downloaded from Metabolomics Workbench'
            }
            return(DE)
        }
        
        if (is(SE,'list')) {
            DE=lapply(SE,as.DatasetExperiment)
            return(DE)
        }
    })

##### DatasetExperiment untargeted
mw_untarg_DE_item = function(name,fields,inputs,parse_fcn,match,...) {
    out=new('mw_untarg_DE_item',
        name=name,
        fields=fields,
        inputs=inputs,
        parse_fcn=parse_fcn,
        match=match,
        locked = c('name','fields','inputs','parse_fcn','match')
    )
    return(out)
}

.mw_untarg_DE_item = setClass(
    Class = 'mw_untarg_DE_item',
    contains = 'mw_output_item'
)

#' @rdname do_query
#' @export
#' @importFrom data.table rbindlist
#' @import httr
#' @import jsonlite
#' @import struct
setMethod(f = 'do_query',
    signature = c('mw_context','mw_input_item','character','mw_untarg_DE_item'),
    definition = function(context,input_item,input_value,output_item) {
        
        # use SE then convert
        SE = do_query(context$name,input_item$name,input_value,'untarg_SummarizedExperiment')
        DE = as.DatasetExperiment(SE)
        return(DE)
    })



##### MultiAssayExperiment
mw_MAE_item = function(name,fields,inputs,parse_fcn,match,...) {
    out=new('mw_MAE_item',
        name=name,
        fields=fields,
        inputs=inputs,
        parse_fcn=parse_fcn,
        match=match,
        locked = c('name','fields','inputs','parse_fcn','match')
    )
    return(out)
}

.mw_MAE_item = setClass(
    Class = 'mw_MAE_item',
    contains = 'mw_output_item'
)

#' @rdname do_query
#' @export
#' @importFrom data.table rbindlist
#' @import httr
#' @import jsonlite
#' @import MultiAssayExperiment
setMethod(f = 'do_query',
    signature = c('mw_context','mw_input_item','character','mw_MAE_item'),
    definition = function(context,input_item,input_value,output_item) {
        err=list()
        # check we have a study context
        if (context$name != 'study') {
            err=c(err,'MultiAssayExperiment output_item can only be used with "study" context')
        }
        if(length(err)>0) {
            stop(err)
        }
        
        # use SE, then convert to DE
        SE = do_query(context$name,input_item$name,input_value,'SummarizedExperiment')
        
        if (is(SE,'SummarizedExperiment')) {
            SE=list(SE)
            names(SE)=input_value
        }
        
        SE=MatchedAssayExperiment(SE,colData=colData(SE[[1]]))
        return(SE)
    })