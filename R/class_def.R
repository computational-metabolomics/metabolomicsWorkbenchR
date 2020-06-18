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

setMethod(f = "$<-",
    signature = c("mw_base"),
    definition = function(x,name,value) {
        
        is_slot = name %in% slotNames(x)
        is_private = name %in% c(x@private,'private')
        is_locked = name %in% c(x@locked,'private','locked')
        
        if (is_slot & !is_private & !is_locked) {
            slot(x,name) = value
            return(x)
        } else {
            if (!is_slot) {
                stop(paste0('"',name,'" is not a valid slot for objects of class "', class(x)[1],'"'))
            }
            if (is_private) {
                stop(paste0('"',name,'" is a private slot for internal use only.'))
            }
            if (is_locked) {
                stop(paste0('"',name,'" is a read-only slot.'))
            }
        }
    }
)


############################## CONTEXTS ######################################

mw_context = function(name,input_items,output_items,allow,...) {
    out = new('mw_context',
        name = name,
        input_items = input_items,
        output_items = output_items,
        allow = allow,
        locked = c('name','input_items','allow','output_items'),
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
        output_items = 'character',
        allow = 'character'
    )
)

#' @export
setMethod(f = 'is_valid',
    signature = c('mw_context','character','character','character'),
    definition = function(context,input_item,input_value,output_item) {
        
        name_valid = context@name %in% c('study','compound','refmet','gene','protein','moverz','exactmass')
        input_valid = all(input_item %in% context@input_items)
        output_valid = all(output_item %in% context@output_items)
        length_valid = !(length(input_value)>1)
        length_out_valid = !(length(output_item)>1 & context@allow=='one')
        
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
            err=c(err,"Length of input_value is limited to 1 for this context.\n")
        }
        if (!length_out_valid) {
            err=c(err,"Length of output_item is limited to 1 for this context.\n")
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
        output_items = '',
        allow = '',
        ion_types = ion_types,
        tol_range = tol_range,
        mz_range = mz_range,
        locked = c('name','input_items','allow','output_items','ion_types','tol_range','mz_range'),
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

#' @export
setMethod(f = 'is_valid',
    signature = c('mw_moverz_context','character','character','missing'),
    definition = function(context,input_item,input_value) {
        
        input_valid = all(input_item %in% context@input_items)
        
        length_valid1 = length(input_value[1])==1 # database
        length_valid2 = length(input_value[2])==1 # mz
        length_valid3 = length(input_value[3])==1 # ion
        length_valid4 = length(input_value[4])==1 # tol
        
        range_valid2 = as.numeric(input_value[2]) >= context@mz_range[1] & as.numeric(input_value[2]) <= context@mz_range[2]
        ion_valid = input_value[3] %in% context@ion_types
        range_valid4 = as.numeric(input_value[4]) >= context@tol_range[1] & as.numeric(input_value[4]) <= context@tol_range[2]
        database_valid=input_value[1] %in% c('LIPIDS','MB','REFMET')
        
        err=list()
        if (!input_valid) {
            err=c(err,paste0('An input_item is not valid for this context.\n'))
        }
        if (!length_valid1) {
            err=c(err,"Length of input_value[1] is limited to 1 for this context.\n")
        }
        if (!length_valid2) {
            err=c(err,"Length of input_value[2] is limited to 1 for this context.\n")
        }
        if (!length_valid3) {
            err=c(err,"Length of input_value[3] is limited to 1 for this context.\n")
        }
        if (!length_valid4) {
            err=c(err,"Length of input_value[4] is limited to 1 for this context.\n")
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
        input_items = '',
        output_items = '',
        allow = '',
        ion_types = ion_types,
        lipid_types = lipid_types,
        locked = c('name','input_items','allow','output_items','ion_types','lipid_types'),
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

#' @export
setMethod(f = 'is_valid',
    signature = c('mw_exactmass_context','character','character','missing'),
    definition = function(context,input_item,input_value) {
        
        
        length_valid1 = length(input_value[1])==1
        length_valid2 = length(input_value[2])==1
        
        
        str=strsplit(input_value[1],'(',fixed=TRUE)[[1]]
        lipid_valid = str[1] %in% context@lipid_types
        ion_valid = input_value[2] %in% context@ion_types
        
        err=list()
        if (!length_valid1) {
            err=c(err,"Length of input_value[1] is limited to 1 for this context.\n")
        }
        if (!length_valid2) {
            err=c(err,"Length of input_value[2] is limited to 1 for this context.\n")
        }
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

mw_input_item = function(name,pattern) {
    out = new('mw_input_item',
        name = name,
        pattern = pattern,
        locked = c('name','pattern')
    )
}

.mw_input_item = setClass(
    Class = 'mw_input_item',
    contains = 'mw_base',
    slots = c(name = 'character',
        pattern = 'list'
    )
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

### do_query methods ###

#' @export
#' @import data.table
#' @import httr
#' @import jsonlite
setMethod(f = 'do_query',
    signature = c('character','character','character','character'),
    definition = function(context,input_item,input_value,output_item) {
        
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
                input_item[[k]]=metabolomicsWorkbenchR::input_item[[input_item]]
            }
        } else {
            input_item=metabolomicsWorkbenchR::input_item[[input_item]]
        }
        
        # query the database
        out=do_query(context,input_item,input_value,output_item)
        
        return(out)
    }
)

#' @export
#' @import data.table
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

        out = use_api(str,output_item,input_value)
        
        return(out)
        
    }
)

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

#' @export
setMethod(f = 'do_query',
    signature = c('mw_moverz_context','list','character','missing'),
    definition = function(context,input_item,input_value,output_item) {
        output_item=metabolomicsWorkbenchR::output_item$moverz
        out = do_query(context,input_item,input_value,output_item)
        return(out)
    }
)

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


#' @import data.table
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

        out = use_api(str,output_item,input_value)
        
        return(out)
        
    }
)

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

#' @export
setMethod(f = 'do_query',
    signature = c('mw_exactmass_context','list','character','missing'),
    definition = function(context,input_item,input_value,output_item) {
        output_item=metabolomicsWorkbenchR::output_item$exactmass
        out = do_query(context,input_item,input_value,output_item)
        return(out)
    }
)

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


#' @export
#' @import data.table
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
        
        out = use_api(str,output_item,input_value)
        
        return(out)
    }
)

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

use_api = function(str,output_item=NULL,input_value=NULL) {
    print(str)
    response = httr::GET(
        url=str
    )
    
    print(response$headers$`content-type`)
    
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