#' @include generics.R parse_fcns.R

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
        
        length_valid1 = length(input_value[1])==1
        length_valid2 = length(input_value[2])==1
        length_valid3 = length(input_value[3])==1
        
        range_valid1 = as.numeric(input_value1) >= context@mz_range[1] & input_value1 <= context@mz_range[2]
        ion_valid = input_value2 %in% context@ion_types
        range_valid3 = as.numeric(input_value3) >= context@tol_range[1] & input_value3 <= context@tol_range[2]
        
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
        if (!range_valid1) {
            err=c(err,"input_value1 is out of range for this context.\n")
        }
        if (!range_valid3) {
            err=c(err,"input_value3 is out of range for this context.\n")
        }
        if (!ion_valid) {
            err=c(err,paste0('"',input_value[2], '" is not a valid ion for this context.\n'))
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
    signature = c('mw_exactmass_context','missing','character','missing'),
    definition = function(context,input_value) {
        
        
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

################## query object #######################

#' @export
mw_query = function(context,input_item,input_value,output_item,...) {
    
    out=new('mw_query',
        context = context,
        input_item = input_item,
        input_value = input_value,
        output_item = output_item,
        ...
    )
    return(out)
}

.mw_query = setClass(
    Class = 'mw_query',
    contains = 'mw_base',
    slots = c(
        context = 'mw_context',
        input_item = 'mw_input_item',
        input_value = 'character',
        output_item = 'mw_output_item'
    )
)

#' @export
#' @import data.table
#' @import httr
setMethod(f = 'do_query',
    signature = 'mw_query',
    definition = function(Q) {
        
        # check the context is valid
        is_valid(Q$context,Q$input_item$name,Q$input_value,Q$output_item$name)
        # check the input pattern
        check_pattern(Q$input_item,Q$input_value,Q$output_item$match)
        
        # build the url
        str=paste('https://www.metabolomicsworkbench.org/rest',
            Q$context@name,
            Q$input_item$name,
            paste(Q$input_value,collapse='/',sep=''),
            paste(Q$output_item$name,collapse=',',sep=''),
            sep='/')
        print(str)
        out = httr::GET(
            url=str
        )
        out=httr::content(out,'parsed')
        
        if (length(out)==0) {
            message('There were no results for your query.')
            return(NULL)
        }
        
        # reformat the parsed content according to the output_item
        out = Q$output_item$parse_fcn(out,Q)
        
        return(out)
    }
)

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

