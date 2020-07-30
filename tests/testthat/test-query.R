test_that("study_context metabolite_id data frame is as expected", {
    
    context=context$study
    input_item=input_item$metabolite_id
    input_value='ME000096'
    output_item=output_item$metabolite_info
    
    df = output_item$parse_fcn(R[[1]],output_item,input_item)

    # number rows
    expect_equal(nrow(df),1) 
    # number columns
    expect_equal(ncol(df),10)
    # metabolite id
    expect_equal(df$metabolite_id[1],'ME000096')#1
    
    
})


test_that("study_context summary data frame is as expected", {
    
    context=context$study
    input_item=input_item$study_id
    input_value='ST000001'
    output_item=output_item$summary
    
    df = output_item$parse_fcn(R[[4]],output_item,input_item)
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),12)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')#5
})

test_that("study_context number_of_metabolites data frame is as expected", {
    
    context=context$study
    input_item=input_item$study_id
    input_value='ST000001'
    output_item=output_item$number_of_metabolites
    
    df = output_item$parse_fcn(R[[5]],output_item,input_item)
    
    # df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='number_of_metabolites')
    
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),5)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')#6
})

test_that("study_context analysis data frame is as expected", {
    # df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='analysis')
    
    context=context$study
    input_item=input_item$study_id
    input_value='ST000001'
    output_item=output_item$analysis
    
    df = output_item$parse_fcn(R[[6]],output_item,input_item)
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),17)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')#7
})

test_that("study_context metabolites data frame is as expected", {
    # df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='metabolites')
    
    context=context$study
    input_item=input_item$study_id
    input_value='ST000001'
    output_item=output_item$metabolites
    
    df = output_item$parse_fcn(R[[7]],output_item,input_item)
    
    # number rows
    expect_equal(nrow(df),102)
    # number columns
    expect_equal(ncol(df),8)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')#8
})

test_that("study_context species data frame is as expected", {
    # df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='species')
    
    context=context$study
    input_item=input_item$study_id
    input_value='ST000001'
    output_item=output_item$species
    
    df = output_item$parse_fcn(R[[8]],output_item,input_item)
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),3)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')#9
})

test_that("study_context source data frame is as expected", {
    # df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='source')
    
    context=context$study
    input_item=input_item$study_id
    input_value='ST000001'
    output_item=output_item$source
    
    df = output_item$parse_fcn(R[[9]],output_item,input_item)
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),2)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')#10
})

test_that("study_context disease data frame is as expected", {
    # df =  do_query(context='study',input_item='study_id',input_value='ST000010',output_item='disease')
    
    context=context$study
    input_item=input_item$study_id
    input_value='ST000010'
    output_item=output_item$disease
    
    df = output_item$parse_fcn(R[[10]],output_item,input_item)
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),2)
    # metabolite id
    expect_equal(df$study_id[1],'ST000010')#11
})

test_that("study_context untarg_studies data frame is as expected", {
    # df =  do_query(context='study',input_item='ignored',input_value='ST000010',output_item='untarg_studies')
    
    context=context$study
    input_item=input_item$study_id
    input_value='ST000010'
    output_item=output_item$untarg_studies
    
    df = output_item$parse_fcn(R[[11]],output_item,input_item)
    
    # number columns
    expect_equal(ncol(df),6)
    # metabolite id
    expect_equal(df$study_id[1],'ST000009')#12
})

test_that("study_context untarg_factors data frame is as expected", {
    # df =  do_query(context='study',input_item='analysis_id',input_value='AN000023',output_item='untarg_factors')
    
    context=context$study
    input_item=input_item$analysis_id
    input_value='AN000023'
    output_item=output_item$untarg_factors
    
    df = output_item$parse_fcn(R[[12]],output_item,input_item)
    
    # number columns
    expect_equal(ncol(df),5)
    # number rows
    expect_equal(nrow(df),24) #13
})

test_that("study_context data data frame is as expected", {
    # df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='data')
    context=context$study
    input_item=input_item$study_id
    input_value='ST000001'
    output_item=output_item$data
    
    df = output_item$parse_fcn(R[[13]],output_item,input_item)
    
    df=df[[1]]
    
    # number columns
    expect_equal(ncol(df),31)
    # number rows
    expect_equal(nrow(df),102) #14
})

test_that("study_context untarg_data data frame is as expected", {
    # df =  do_query(context='study',input_item='analysis_id',input_value='AN000023',output_item='untarg_data')
    
    context=context$study
    input_item=input_item$analysis_id
    input_value='AN000023'
    output_item=output_item$untarg_data
    
    df = output_item$parse_fcn(R[[14]],output_item,input_item)
    
    # number columns
    expect_equal(ncol(df),4485)
    # number rows
    expect_equal(nrow(df),114)
    
    # check parse of factors
    expect_equal(colnames(df)[1],'AFTER_MEAL_TIME') #15
    
})

test_that("study_context datatable data frame is as expected", {
    # df =  do_query(context='study',input_item='analysis_id',input_value='AN000023',output_item='datatable')
    
    context=context$study
    input_item=input_item$analysis_id
    input_value='AN000023'
    output_item=output_item$datatable
    
    df = output_item$parse_fcn(R[[15]],output_item,input_item)
    
    # number columns
    expect_equal(ncol(df),252)
    # number rows
    expect_equal(nrow(df),114)
    
    # check parse of factors
    expect_equal(colnames(df)[1],'AFTER_MEAL_TIME') #16
    
})

test_that("compound_context compound_exact data frame is as expected", {
    # df =  do_query(context='compound',input_item='regno',input_value='11',output_item='compound_exact')
    
    context=context$compound
    input_item=input_item$regno
    input_value='11'
    output_item=output_item$compound_exact
    
    df = output_item$parse_fcn(R[[16]],output_item,input_item)
    
    # number columns
    expect_equal(ncol(df),13)
    # number rows
    expect_equal(nrow(df),1)
    
    # check parse of factors
    expect_equal(df$regno[1],'11') #17
    
})

test_that("compound_context classification data frame is as expected", {
    # df =  do_query(context='compound',input_item='regno',input_value='11',output_item='classification')
    
    context=context$compound
    input_item=input_item$regno
    input_value='11'
    output_item=output_item$classification
    
    df = output_item$parse_fcn(R[[17]],output_item,input_item)
    
    # number columns
    expect_equal(ncol(df),12)
    # number rows
    expect_equal(nrow(df),1)
    
    # check parse of factors
    expect_equal(df$regno[1],'11') #18
    
})

test_that("protein_context protein_exact data frame is as expected", {
    # df =  do_query(context='protein',input_item='uniprot_id',input_value='Q13085',output_item='protein_exact')
    
    context=context$protein
    input_item=input_item$uniprot_id
    input_value='Q13085'
    output_item=output_item$protein_exact
    
    df = output_item$parse_fcn(R[[18]],output_item,input_item)
    
    # number columns
    expect_equal(ncol(df),16)
    # number rows
    expect_equal(nrow(df),5)
    
    # check parse of factors
    expect_equal(df$uniprot_id[1],'Q13085') #19
})

test_that("gene_context gene_exact data frame is as expected", {
    # df =  do_query(context='gene',input_item='mgp_id',input_value='MGP000016',output_item='gene_exact')
    
    context=context$gene
    input_item=input_item$mgp_id
    input_value='MGP000016'
    output_item=output_item$gene_exact
    
    df = output_item$parse_fcn(R[[19]],output_item,input_item)
    
    # number columns
    expect_equal(ncol(df),12)
    # number rows
    expect_equal(nrow(df),1)
    
    # check parse of factors
    expect_equal(df$mgp_id[1],'MGP000016') #20
})

test_that("refmet_context name data frame is as expected", {
    # df =  do_query(context='refmet',input_item='name',input_value='Cholesterol',output_item='refmet_exact')
    
    context=context$refmet
    input_item=input_item$name
    input_value='Cholesterol'
    output_item=output_item$refmet_exact
    
    df = output_item$parse_fcn(R[[20]],output_item,input_item)
    
    # number columns
    expect_equal(ncol(df),10)
    # number rows
    expect_equal(nrow(df),1)
    
    # check parse of factors
    expect_equal(df$name[1],'Cholesterol') #21
})

test_that("moverz_context name data frame is as expected for mb", {
     #df =  do_query(context='moverz',
     #   input_item=c('database','mz','ion','tolerance'),
     #   input_value=c('MB','635.52','M+H','0.5'))
    
    context=context$moverz
    input_item=input_item$moverz
    input_value=list('MB','635.52','M+H','0.5')
    output_item=output_item$moverz
    
    df = output_item$parse_fcn(R[[21]],output_item,input_value)
    
    # number columns
    expect_equal(ncol(df),10)
    # number rows
    expect_equal(nrow(df),74) #22
})

test_that("moverz_context name data frame is as expected for lipids", {
    #df =  do_query(context='moverz',
    #    input_item=c('database','mz','ion','tolerance'),
    #    input_value=c('LIPIDS','513.45','M-2H','0.2'))
    
    context=context$moverz
    input_item=input_item$moverz
    input_value=list('LIPIDS','513.45','M-2H','0.2')
    output_item=output_item$moverz
    
    df = output_item$parse_fcn(R[[22]],output_item,input_value)
    
    # number columns
    expect_equal(ncol(df),6)
    # number rows
    expect_equal(nrow(df),50) #23
})

test_that("moverz_context name data frame is as expected for refmet", {
    #df =  do_query(context='moverz',
    #    input_item=c('database','mz','ion','tolerance'),
    #    input_value=c('REFMET','255.2','M+H','0.2'))
    
    context=context$moverz
    input_item=input_item$moverz
    input_value=list('REFMET','255.2','M+H','0.2')
    output_item=output_item$moverz
    
    df = output_item$parse_fcn(R[[23]],output_item,input_value)
    
    # number columns
    expect_equal(ncol(df),10)
    # number rows
    expect_equal(nrow(df),33) #24
})

test_that("exactmass_context name data frame is as expected", {
    #df =  do_query(context='exactmass',
    #    input_item=c('lipid','ion'),
    #    input_value=c('PC(34:1)','M+H')
    #)
    
    context=context$exactmass
    input_item=input_item$exactmass
    input_value=list('PC(34:1)','M+H')
    output_item=output_item$exactmass
    
    df = output_item$parse_fcn(R[[24]],output_item,input_value)
    
    # number columns
    expect_equal(ncol(df),4)
    # number rows
    expect_equal(nrow(df),1) #25
})