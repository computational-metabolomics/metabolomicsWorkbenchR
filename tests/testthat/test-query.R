test_that("study_context metabolite_id data frame is as expected", {
    Q = mw_query(context=context$study,input_item=input_item$metabolite_id,input_value='ME000096',output_item=output_item$metabolite_info)
    df = do_query(Q)
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),10)
    # metabolite id
    expect_equal(df$metabolite_id[1],'ME000096')
    
    
})

test_that("study_context metabolite_id data frame is as expected when using strings", {
    Q = mw_query(context='study',input_item='metabolite_id',input_value='ME000096',output_item='metabolite_info')
    df = do_query(Q)
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),10)
    # metabolite id
    expect_equal(df$metabolite_id[1],'ME000096')
    
    
})

test_that("pattern matching works", {
    Q = mw_query(context=context$study,input_item=input_item$metabolite_id,input_value='ME',output_item=output_item$metabolite_info)
    expect_error(do_query(Q),"The input_value does not match the required pattern for the provided input_item and output_item.",fixed=TRUE)
})

test_that("no results returns NULL", {
    Q = mw_query(context=context$study,input_item=input_item$metabolite_id,input_value='ME000000',output_item=output_item$metabolite_info)
    expect_null(do_query(Q))
})

test_that("study_context summary data frame is as expected when using strings", {
    Q = mw_query(context='study',input_item='study_id',input_value='ST000001',output_item='summary')
    df = do_query(Q)
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),12)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')
})

test_that("study_context number_of_metabolites data frame is as expected when using strings", {
    Q = mw_query(context='study',input_item='study_id',input_value='ST000001',output_item='number_of_metabolites')
    df = do_query(Q)
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),5)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')
})

test_that("study_context analysis data frame is as expected when using strings", {
    Q = mw_query(context='study',input_item='study_id',input_value='ST000001',output_item='analysis')
    df = do_query(Q)
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),17)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')
})

test_that("study_context metabolites data frame is as expected when using strings", {
    Q = mw_query(context='study',input_item='study_id',input_value='ST000001',output_item='metabolites')
    df = do_query(Q)
    
    # number rows
    expect_equal(nrow(df),102)
    # number columns
    expect_equal(ncol(df),8)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')
})

test_that("study_context species data frame is as expected when using strings", {
    Q = mw_query(context='study',input_item='study_id',input_value='ST000001',output_item='species')
    df = do_query(Q)
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),3)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')
})

test_that("study_context source data frame is as expected when using strings", {
    Q = mw_query(context='study',input_item='study_id',input_value='ST000001',output_item='source')
    df = do_query(Q)
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),2)
    # metabolite id
    expect_equal(df$study_id[1],'ST000001')
})

test_that("study_context disease data frame is as expected when using strings", {
    Q = mw_query(context='study',input_item='study_id',input_value='ST000010',output_item='disease')
    df = do_query(Q)
    
    # number rows
    expect_equal(nrow(df),1)
    # number columns
    expect_equal(ncol(df),2)
    # metabolite id
    expect_equal(df$study_id[1],'ST000010')
})

test_that("study_context untarg_studies data frame is as expected when using strings", {
    Q = mw_query(context='study',input_item='ignored',input_value='ST000010',output_item='untarg_studies')
    df = do_query(Q)
    
    # number columns
    expect_equal(ncol(df),6)
    # metabolite id
    expect_equal(df$study_id[1],'ST000009')
})

test_that("study_context untarg_factors data frame is as expected when using strings", {
    Q = mw_query(context='study',input_item='analysis_id',input_value='AN000023',output_item='untarg_factors')
    df = do_query(Q)
    
    # number columns
    expect_equal(ncol(df),5)
    # number rows
    expect_equal(nrow(df),24)
})

test_that("study_context data data frame is as expected when using strings", {
    Q = mw_query(context='study',input_item='study_id',input_value='ST000001',output_item='data')
    df = do_query(Q)
    
    # number columns
    expect_equal(ncol(df),31)
    # number rows
    expect_equal(nrow(df),102)
})

test_that("study_context untarg_data data frame is as expected when using strings", {
    Q = mw_query(context='study',input_item='analysis_id',input_value='AN000023',output_item='untarg_data')
    df = do_query(Q)
    
    # number columns
    expect_equal(ncol(df),4486)
    # number rows
    expect_equal(nrow(df),114)
    
    # check parse of factors
    expect_equal(colnames(df)[2],'AFTER_MEAL_TIME')
    
})

test_that("study_context datatable data frame is as expected when using strings", {
    Q = mw_query(context='study',input_item='analysis_id',input_value='AN000023',output_item='datatable')
    df = do_query(Q)
    
    # number columns
    expect_equal(ncol(df),253)
    # number rows
    expect_equal(nrow(df),114)
    
    # check parse of factors
    expect_equal(colnames(df)[2],'AFTER_MEAL_TIME')
    
})