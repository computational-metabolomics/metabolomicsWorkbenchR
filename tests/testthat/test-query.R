test_that("study_context metabolite_id data frame is as expected", {
    context=context$study
    input_item=input_item$metabolite_id
    input_value='ME000096'
    output_item=output_item$metabolite_info
    
    df = do_query(context,input_item, input_value, output_item)
    
    # number rows
    expect_equal(nrow(df),1) #1
    # number columns
    expect_equal(ncol(df),10)#2
    # metabolite id
    expect_equal(df$metabolite_id[1],'ME000096')#3
    
    
})

test_that("study_context metabolite_id data frame is as expected when using strings", {
    df = do_query(context='study',input_item='metabolite_id',input_value='ME000096',output_item='metabolite_info')
    
    # number rows
    expect_equal(nrow(df),1)#4
    # number columns
    expect_equal(ncol(df),10)#5
    # metabolite id
    expect_equal(df$metabolite_id[1],'ME000096')#6
    
    
})

test_that("pattern matching works", {
    expect_error(
        do_query(
            context=metabolomicsWorkbenchR::context$study,
            input_item=metabolomicsWorkbenchR::input_item$metabolite_id,
            input_value='ME',
            output_item=metabolomicsWorkbenchR::output_item$metabolite_info
        ),
        "The input_value does not match the required pattern for the provided input_item and output_item.",
        fixed=TRUE
    )#7
})

test_that("no results returns NULL", {
    expect_null(
        do_query(
            context=metabolomicsWorkbenchR::context$study,
            input_item=metabolomicsWorkbenchR::input_item$metabolite_id,
            input_value='ME000000',
            output_item=metabolomicsWorkbenchR::output_item$metabolite_info
        )
    )#8
})

test_that("study_context summary data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='summary')
    
    
    # number rows
    expect_equal(nrow(df),1)#9
    # number columns
    expect_equal(ncol(df),12)#10
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')#11
})

test_that("study_context number_of_metabolites data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='number_of_metabolites')
    
    
    # number rows
    expect_equal(nrow(df),1)#13
    # number columns
    expect_equal(ncol(df),5)#14
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')#15
})

test_that("study_context analysis data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='analysis')
    
    
    # number rows
    expect_equal(nrow(df),1)#16
    # number columns
    expect_equal(ncol(df),17)#17
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')#18
})

test_that("study_context metabolites data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='metabolites')
    
    
    # number rows
    expect_equal(nrow(df),102)#19
    # number columns
    expect_equal(ncol(df),8)#20
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')#21
})

test_that("study_context species data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='species')
    
    
    # number rows
    expect_equal(nrow(df),1)#22
    # number columns
    expect_equal(ncol(df),3)#23
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')#24
})

test_that("study_context source data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='source')
    
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),2)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')
})

test_that("study_context disease data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='study_id',input_value='ST000010',output_item='disease')
    
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),2)
    # metabolite id
    expect_equal(df$study_id[1],'ST000010')
})

test_that("study_context untarg_studies data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='ignored',input_value='ST000010',output_item='untarg_studies')
    
    
    # number columns
    expect_equal(ncol(df),6)
    # metabolite id
    expect_equal(df$study_id[1],'ST000009')
})

test_that("study_context untarg_factors data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='analysis_id',input_value='AN000023',output_item='untarg_factors')
    
    
    # number columns
    expect_equal(ncol(df),5)
    # number rows
    expect_equal(nrow(df),24)
})

test_that("study_context data data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='data')
    df=df[[1]]
    
    # number columns
    expect_equal(ncol(df),31)
    # number rows
    expect_equal(nrow(df),102)
})

test_that("study_context untarg_data data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='analysis_id',input_value='AN000023',output_item='untarg_data')
    
    # number columns
    expect_equal(ncol(df),4485)
    # number rows
    expect_equal(nrow(df),114)
    
    # check parse of factors
    expect_equal(colnames(df)[1],'AFTER_MEAL_TIME')
    
})

test_that("study_context datatable data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='analysis_id',input_value='AN000023',output_item='datatable')
    
    
    # number columns
    expect_equal(ncol(df),252)
    # number rows
    expect_equal(nrow(df),114)
    
    # check parse of factors
    expect_equal(colnames(df)[1],'AFTER_MEAL_TIME')
    
})

test_that("compound_context compound_exact data frame is as expected when using strings", {
    df =  do_query(context='compound',input_item='regno',input_value='11',output_item='compound_exact')
    
    
    # number columns
    expect_equal(ncol(df),13)
    # number rows
    expect_equal(nrow(df),1)
    
    # check parse of factors
    expect_equal(df$regno[1],'11')
    
})

test_that("compound_context classification data frame is as expected when using strings", {
    df =  do_query(context='compound',input_item='regno',input_value='11',output_item='classification')
    
    
    # number columns
    expect_equal(ncol(df),12)
    # number rows
    expect_equal(nrow(df),1)
    
    # check parse of factors
    expect_equal(df$regno[1],'11')
    
})

test_that("protein_context protein_exact data frame is as expected when using strings", {
    df =  do_query(context='protein',input_item='uniprot_id',input_value='Q13085',output_item='protein_exact')
    
    
    # number columns
    expect_equal(ncol(df),16)
    # number rows
    expect_equal(nrow(df),5)
    
    # check parse of factors
    expect_equal(df$uniprot_id[1],'Q13085')
})

test_that("gene_context gene_exact data frame is as expected when using strings", {
    df =  do_query(context='gene',input_item='mgp_id',input_value='MGP000016',output_item='gene_exact')
    
    
    # number columns
    expect_equal(ncol(df),12)
    # number rows
    expect_equal(nrow(df),1)
    
    # check parse of factors
    expect_equal(df$mgp_id[1],'MGP000016')
})

test_that("refmet_context name data frame is as expected when using strings", {
    df =  do_query(context='refmet',input_item='name',input_value='Cholesterol',output_item='refmet_exact')
    
    # number columns
    expect_equal(ncol(df),10)
    # number rows
    expect_equal(nrow(df),1)
    
    # check parse of factors
    expect_equal(df$name[1],'Cholesterol')
})

test_that("moverz_context name data frame is as expected when using strings for mb", {
    df =  do_query(context='moverz',
        input_item=c('database','mz','ion','tolerance'),
        input_value=c('MB','635.52','M+H','0.5'))
    
    # number columns
    expect_equal(ncol(df),10)
    # number rows
    expect_equal(nrow(df),74)
})

test_that("moverz_context name data frame is as expected when using strings for lipids", {
    df =  do_query(context='moverz',
        input_item=c('database','mz','ion','tolerance'),
        input_value=c('LIPIDS','513.45','M-2H','0.2'))
    
    # number columns
    expect_equal(ncol(df),6)
    # number rows
    expect_equal(nrow(df),50)
})

test_that("moverz_context name data frame is as expected when using strings for refmet", {
    df =  do_query(context='moverz',
        input_item=c('database','mz','ion','tolerance'),
        input_value=c('REFMET','255.2','M+H','0.2'))
    
    # number columns
    expect_equal(ncol(df),10)
    # number rows
    expect_equal(nrow(df),33)
})

test_that("exactmass_context name data frame is as expected when using strings", {
    df =  do_query(context='exactmass',
        input_item=c('lipid','ion'),
        input_value=c('PC(34:1)','M+H')
    )
    
    # number columns
    expect_equal(ncol(df),4)
    # number rows
    expect_equal(nrow(df),1)
})