test_that("study_context metabolite_id data frame is as expected", {
    context=context$study
    input_item=input_item$metabolite_id
    input_value='ME000096'
    output_item=output_item$metabolite_info
    
    df = do_query(context,input_item, input_value, output_item)
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),10)
    # metabolite id
    expect_equal(df$metabolite_id[1],'ME000096')
    
    
})

test_that("study_context metabolite_id data frame is as expected when using strings", {
    df = do_query(context='study',input_item='metabolite_id',input_value='ME000096',output_item='metabolite_info')
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),10)
    # metabolite id
    expect_equal(df$metabolite_id[1],'ME000096')
    
    
})

test_that("pattern matching works", {
    expect_error(
        do_query(
            context=context$study,
            input_item=input_item$metabolite_id,
            input_value='ME',
            output_item=output_item$metabolite_info
        ),
        "The input_value does not match the required pattern for the provided input_item and output_item.",
        fixed=TRUE
    )
})

test_that("no results returns NULL", {
    expect_null(
        do_query(
            context=context$study,
            input_item=input_item$metabolite_id,
            input_value='ME000000',
            output_item=output_item$metabolite_info
        )
    )
})

test_that("study_context summary data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='summary')
    
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),12)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')
})

test_that("study_context number_of_metabolites data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='number_of_metabolites')
    
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),5)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')
})

test_that("study_context analysis data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='analysis')
    
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),17)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')
})

test_that("study_context metabolites data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='metabolites')
    
    
    # number rows
    expect_equal(nrow(df),102)
    # number columns
    expect_equal(ncol(df),8)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')
})

test_that("study_context species data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='study_id',input_value='ST000001',output_item='species')
    
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),3)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')
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
    
    
    # number columns
    expect_equal(ncol(df),31)
    # number rows
    expect_equal(nrow(df),102)
})

test_that("study_context untarg_data data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='analysis_id',input_value='AN000023',output_item='untarg_data')
    
    
    # number columns
    expect_equal(ncol(df),4486)
    # number rows
    expect_equal(nrow(df),114)
    
    # check parse of factors
    expect_equal(colnames(df)[2],'AFTER_MEAL_TIME')
    
})

test_that("study_context datatable data frame is as expected when using strings", {
    df =  do_query(context='study',input_item='analysis_id',input_value='AN000023',output_item='datatable')
    
    
    # number columns
    expect_equal(ncol(df),253)
    # number rows
    expect_equal(nrow(df),114)
    
    # check parse of factors
    expect_equal(colnames(df)[2],'AFTER_MEAL_TIME')
    
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