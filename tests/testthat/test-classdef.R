test_that("context creation works as expected", {
    
    study=metabolomicsWorkbenchR:::mw_context(
        name = 'study',
        input_items = c('study_id','study_title','institute','last_name','analysis_id','metabolite_id'),
        output_items = c('summary','factors','analysis','metabolites','mwtab',
            'source','species','disease','number_of_metabolites','data','datatable',
            'untarg_studies','untarg_factors','untarg_data','metabolite_info',
            'SummarizedExperiment')
    )
    
    expect_equal(study,context$study)
})

test_that("input_item creation works as expected", {
    
    study_id=metabolomicsWorkbenchR:::mw_input_item(
        name = 'study_id',
        pattern = list(
            exact='^ST[0-9]{6}$',
            partial='^S(T[0-9]{0,6})?$'),
        example=c('ST000001','ST')
    )
    
    expect_equal(study_id,input_item$study_id)
})

test_that("output_item creation works as expected", {
    
    metabolite_info=metabolomicsWorkbenchR:::mw_output_item(
        name='summary',
        fields=c('metabolite_id','metabolite_name','refmet_name',
            'pubchem_id','other_id','other_id_type','kegg_id',
            'ri','ri_type','moverz_quant'
        ),
        inputs=c('metabolite_id'),
        match='exact',
        parse_fcn=metabolomicsWorkbenchR:::parse_data_frame
    )
    
    expect_equal(metabolite_info$name,output_item$metabolite_info$name)
})

test_that("error when slot not valid",{
    expect_error(output_item$summary$banana)
})

test_that("access to private slots is restricted",{
    expect_error(output_item$summary$private)
})

test_that("base class constructor works", {
    b=metabolomicsWorkbenchR:::mw_base(private='private',locked='locked')
    s=slotNames(b)
    expect_equal(s,c('private','locked'))
})

test_that("show works for classes", {
    expect_output(show(context$study),'A Metabolomics Workbench "context"')
    expect_output(show(input_item$study_id),'A Metabolomics Workbench "input_item"')
    expect_output(show(output_item$summary),'A Metabolomics Workbench "output_item"')
})


test_that("is_valid works for contexts",{
    # TRUE case
    expect_true(is_valid(context$study,'study_id','ST000001','summary'))
    
    # not a valid context
    study=metabolomicsWorkbenchR:::mw_context(
        name = 'banana',
        input_items = c('study_id'),
        output_items = c('summary')
    )
    expect_error(is_valid(study,'study_id','ST000001','summary'))
    
    # not a valid input
    expect_error(is_valid(context$study,'banana','ST000001','summary'))
    
    # not a valid output item
    expect_error(is_valid(context$study,'study_id','ST000001','banana'))
    
    # invalid length
    expect_error(is_valid(context$study,'study_id',c('ST000001','ST000002'),'summary'))
    expect_error(is_valid(context$study,c('study_id','analysis_id'),'ST000001','summary'))
})

test_that('moverz_context constructor works',{
    moverz = metabolomicsWorkbenchR:::mw_moverz_context(
        input_items = c('database','mz','ion','tolerance'),
        mz_range = c(50,2000),
        tol_range = c(0.0001,1),
        ion_types = c('M+H','M+H-H2O','M+2H','M+3H','M+4H','M+K',
            'M+2K','M+Na','M+2Na','M+Li','M+2Li','M+NH4','M+H+CH3CN','M+Na+CH3CN','M.NaFormate+H',
            'M.NH4Formate+H','M.CH3','M.TMSi','M.tBuDMSi','M-H','M-H-H2O','M+Na-2H','M+K-2H','M-2H','M-3H','M4H','M.Cl','M.F','M.HF2','M.OAc','M.Formate','M.NaFormate-H','M.NH4Formate-H','Neutral')
    )
    expect_equal(moverz,context$moverz)
})

test_that("is_valid works for moverz contexts",{
    # TRUE case
    expect_true(is_valid(context$moverz,c('database','mz','ion','tolerance'),c('MB','635.52','M+H','0.5')))
    
    # not a valid input_item
    expect_error(is_valid(context$moverz,c('banana','mz','ion','tolerance'),c('MB','635.52','M+H','0.5')))

    # mz out of range
    expect_error(is_valid(context$moverz,c('database','mz','ion','tolerance'),c('MB','1','M+H','0.5')))
    
    # tolerance out of range
    expect_error(is_valid(context$moverz,c('database','mz','ion','tolerance'),c('MB','635.52','M+H','0')))
    
    # not a valid ion
    expect_error(is_valid(context$moverz,c('database','mz','ion','tolerance'),c('MB','635.52','banana','0.5')))
    
    # not a valid database
    expect_error(is_valid(context$moverz,c('database','mz','ion','tolerance'),c('banana','635.52','M+H','0.5')))
    
})

test_that('exactmass_context constructor works',{
    exactmass = metabolomicsWorkbenchR:::mw_exactmass_context(
        ion_types = c('Neutral','M+H','M+H-H2O','M+2H','M+3H',
            'M+4H','M+K','M+2K','M+2K-H','M+Na','M+2Na','M+2Na-H','M+Li',
            'M+2Li','M+Ag','M+NH4','M-H','M-CH3','M2H','M-3H','M-4H','M.Cl',
            'M.OAc','M.Formate'),
        lipid_types = c('ArthroCer','asialoGM2Cer','CAR','CE','Cer','CerP','CoA',
            'DG','DGDG','FA','GalCer','GB3Cer','GlcCer','GM3Cer','GM4Cer','iGB3Cer',
            'LacCer','Lc3Cer','Manb1-4GlcCer','MG','MGDG','MolluCer','PA','PC',
            'PE','PE-Cer','PG','PGP','PI','PI-Cer','PIP','PIP2',
            'PIP3','PS','SM','SQDG','TG')
    )
    expect_equal(exactmass,context$exactmass)
})

test_that("is_valid works for exactmass contexts",{
    # TRUE case
    expect_true(is_valid(context$exactmass,c('lipid','ion'),c('PC(34:1)','M+H')))
    
    # not a valid ion
    expect_error(is_valid(context$exactmass,c('lipid','ion'),c('PC(34:1)','banana')))
    
    # not a valid lipid
    expect_error(is_valid(context$exactmass,c('lipid','ion'),c('banana','M+H')))
    
})

test_that("check input patten works",{
    # TRUE partial match
    expect_true(check_pattern(input_item$study_id,'ST00001','partial'))
    # fail exact match
    expect_error(check_pattern(input_item$study_id,'ST00001','exact'))
    # correct exact match
    expect_true(check_pattern(input_item$study_id,'ST000001','exact'))
    # fail partial match
    expect_error(check_pattern(input_item$study_id,'banana','partial'))
})


test_that("input and output checking works",{
    # valid case
    expect_true(check_puts(input_item$study_id,output_item$summary))
    # not valid case
    expect_error(check_puts(input_item$study_id,output_item$compound_exact))
})

test_that("SummarizedExeperiment and derivatives output_items work",{
    # SE
    SE=mw_SE_item(
        name='SummarizedExperiment',
        fields='',
        inputs=c('study_id','analysis_id'),
        match='exact',
        parse_fcn=function(response,output_item,input_value){}
    )
    expect_true(SE$name=='SummarizedExperiment')
    
    uSE=mw_untarg_SE_item(
        name='untarg_SummarizedExperiment',
        fields='',
        inputs=c('analysis_id'),
        match='exact',
        parse_fcn=function(response,output_item,input_value){}
    )
    expect_true(uSE$name=='untarg_SummarizedExperiment')
    
    # DE
    DE=mw_DE_item(
        name='DatasetExperiment',
        fields='',
        inputs=c('study_id','analysis_id'),
        match='exact',
        parse_fcn=function(response,output_item,input_value){}
    )
    expect_true(DE$name=='DatasetExperiment')
    
    uDE=mw_untarg_DE_item(
        name='untarg_DatasetExperiment',
        fields='',
        inputs=c('analysis_id'),
        match='exact',
        parse_fcn=function(response,output_item,input_value){}
    )
    expect_true(uDE$name=='untarg_DatasetExperiment')
    
    # MAE
    MAE=mw_MAE_item(
        name='MultiAssayExperiment',
        fields='',
        inputs=c('study_id'),
        match='exact',
        parse_fcn=function(response,output_item,input_value){}
    )
    expect_true(MAE$name=='MultiAssayExperiment')
})

