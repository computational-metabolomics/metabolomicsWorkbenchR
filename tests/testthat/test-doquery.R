test_that("do_query works for contexts",{
    Sys.setenv("TESTTHAT"="true")
    
    # simple contexts
    df= do_query('study','study_id','ST000001','summary')
    expect_true(df$study_id[1]=='ST000001')
    
    # moverz context
    df =  do_query(context='moverz',
           input_item=c('database','mz','ion','tolerance'),
           input_value=c('MB','635.52','M+H','0.5'))
    expect_equal(ncol(df),10)
    expect_equal(nrow(df),74) #22
    
    # exactmass context
    df =  do_query(context='exactmass',
        input_item=c('lipid','ion'),
        input_value=c('PC(34:1)','M+H')
    )
    expect_equal(ncol(df),4)
    expect_equal(nrow(df),1)
    
    # SummarizedExepriment
    SE = do_query('study','study_id','ST000001','SummarizedExperiment')
    expect_true(is(SE,'SummarizedExperiment'))
    SE = do_query('study','analysis_id','AN000023','SummarizedExperiment')
    expect_true(is(SE,'SummarizedExperiment'))
    SE = do_query('study','analysis_id','AN000023','SummarizedExperiment')
    expect_true(is(SE,'SummarizedExperiment'))
    
    # DatasetExperiment
    DE = do_query('study','study_id','ST000001','DatasetExperiment')
    expect_true(is(DE,'DatasetExperiment'))
    SE = do_query('study','analysis_id','AN000023','DatasetExperiment')
    expect_true(is(SE,'DatasetExperiment'))
    SE = do_query('study','analysis_id','AN000023','untarg_DatasetExperiment')
    expect_true(is(DE,'DatasetExperiment'))
    
    
    # MultiAssayExperiment
    MAE = do_query('study','study_id','ST000001','MultiAssayExperiment')
    expect_true(is(MAE,'MultiAssayExperiment'))  
    
    Sys.setenv("TESTTHAT"="")
})