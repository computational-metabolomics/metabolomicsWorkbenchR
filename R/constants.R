#' Output items
#'
#' A predefined list of mw_output_item objects. The items have been created to
#' mirror the Metabolomics Workbench API documentation output items as closely 
#' as possible.
#' @examples
#' # list available output_items
#' names(output_item)
#' 
#' # get the output item 'summary'
#' output_item$summary
#' 
#' @rdname output_item
#' @include generics.R class_def.R parse_fcns.R
#'
#' @export
output_item = list()

#' Input items
#'
#' A predefined list of mw_input_item objects. The items have been created to
#' mirror the Metabolomics Workbench API documentation input items as closely 
#' as possible.
#' @examples
#' # list available input_items
#' names(input_item)
#' 
#' # get the input item 'study_id'
#' input_item$study_id
#' 
#' @rdname input_item
#' @export
input_item = list()

#' Contexts
#'
#' A predefined list of mw_context objects. The context have been created to
#' mirror the metabolomics workbench API documentation contexts as closely 
#' as possible.
#' @examples
#' # list available contexts
#' names(context)
#' 
#' # get the context 'study'
#' context$study
#' 
#' @rdname context
#' @export
context = list()

################################## CONTEXTS ###################################

context$study = mw_context(
    name = 'study',
    input_items = c('study_id','study_title','institute','last_name','analysis_id','metabolite_id'),
    output_items = c('summary','factors','analysis','metabolites','mwtab',
        'source','species','disease','number_of_metabolites','data','datatable',
        'untarg_studies','untarg_factors','untarg_data','metabolite_info',
        'SummarizedExperiment')
)

context$compound = mw_context(
    name = 'compound',
    input_items = c('regno','formula','inchi_key','lm_id','pubchem_cid','hmdb_id','kegg_id','chebi_id','metacyc_id','abbrev'),
    output_items = c('all','classification','molfile','png','compound_exact')
)    

context$refmet = mw_context(
    name = 'refmet',
    input_items = c('match','name','inchi_key','pubchem_cid','formula','main_class','sub_class','super_class'),
    output_items = c('all','ignored','refmet_exact')
)

context$gene = mw_context(
    name = 'gene',
    input_items = c('mgp_id','gene_id','gene_name','gene_symbol','taxid'),
    output_items = c('all','gene_exact','gene_partial')
)

context$protein = mw_context(
    name = 'protein',
    input_items = c('mgp_id','gene_id','gene_name','gene_symbol','taxid','mrna_id','refseq_id',
        'protein_gi','uniprot_id','protein_entry','protein_name'),
    output_items = c('all','protein_exact','protein_partial')
)

context$moverz = mw_moverz_context(
    input_items = c('database','mz','ion','tolerance'),
    mz_range = c(50,2000),
    tol_range = c(0.0001,1),
    ion_types = c('M+H','M+H-H2O','M+2H','M+3H','M+4H','M+K',
        'M+2K','M+Na','M+2Na','M+Li','M+2Li','M+NH4','M+H+CH3CN','M+Na+CH3CN','M.NaFormate+H',
        'M.NH4Formate+H','M.CH3','M.TMSi','M.tBuDMSi','M-H','M-H-H2O','M+Na-2H','M+K-2H','M-2H','M-3H','M4H','M.Cl','M.F','M.HF2','M.OAc','M.Formate','M.NaFormate-H','M.NH4Formate-H','Neutral')
)

context$exactmass = mw_exactmass_context(
    ion_types = c('Neutral','M+H','M+H-H2O','M+2H','M+3H',
        'M+4H','M+K','M+2K','M+2K-H','M+Na','M+2Na','M+2Na-H','M+Li','M+2Li','M+Ag','M+NH4','M-H','M-CH3','M2H','M-3H','M-4H','M.Cl','M.OAc','M.Formate'),
    lipid_types = c('ArthroCer','asialoGM2Cer','CAR','CE','Cer','CerP','CoA','DG','DGDG','FA','GalCer','GB3Cer','GlcCer','GM3Cer','GM4Cer','iGB3Cer',
        'LacCer','Lc3Cer','Manb1-4GlcCer','MG','MGDG','MolluCer','PA','PC','PE','PE-Cer','PG','PGP','PI','PI-Cer','PIP','PIP2',
        'PIP3','PS','SM','SQDG','TG')
)


############################## STUDY CONTEXT ##################################

#### study context ######
input_item$study_id = mw_input_item(
    name = 'study_id',
    pattern = list(
        exact='^ST[0-9]{6}$',
        partial='^S(T[0-9]{0,6})?$'),
    example=c('ST000001','ST')
)
input_item$analysis_id = mw_input_item(
    name = 'analysis_id',
    pattern = list(
        exact='^AN[0-9]{6}$',
        partial='^A(N[0-9]{0,6})?$'),
    example=c('AN000021','AN')
)
input_item$metabolite_id = mw_input_item(
    name = 'metabolite_id',
    pattern = list(
        exact='^ME[0-9]{6}$',
        partial='^M(E[0-9]{0,6})?$'),
    example=c('ME000001','ME')
)
input_item$study_title = mw_input_item(
    name = 'study_title',
    pattern = list(
        exact='*',
        partial='*'),
    example=c('Diabetes')
)
input_item$institute = mw_input_item(
    name = 'institute',
    pattern = list(
        exact='*',
        partial='*'),
    example=c('University')
)
input_item$last_name = mw_input_item(
    name = 'last_name',
    pattern = list(
        exact='*',
        partial='*'),
    example=c('Kind')
)
input_item$ignored = mw_input_item(
    name = 'study_id',
    pattern = list(
        exact='*',
        partial='*'),
    example='This is a special input item that is ignored for certain contexts and output items.'
)
output_item$metabolite_info=mw_output_item(
    name='summary',
    fields=c('metabolite_id','metabolite_name','refmet_name',
        'pubchem_id','other_id','other_id_type','kegg_id',
        'ri','ri_type','moverz_quant'
    ),
    inputs=c('metabolite_id'),
    match='exact',
    parse_fcn=parse_data_frame
)
output_item$number_of_metabolites=mw_output_item(
    name='number_of_metabolites',
    fields=c('study_id','analysis_id','num_metabolites','analysis_display','study_title'),
    inputs=c('study_id','study_title','institute','last_name'),
    match='partial',
    parse_fcn=parse_data_frame
)
output_item$summary=mw_output_item(
    name='summary',
    fields=c('study_id','study_title','study_type','institute',
        'department','last_name','first_name','email','phone',
        'submit_date','study_summary','subject_species'),
    inputs=c('study_id','study_title','institute','last_name'),
    match='partial',
    parse_fcn=parse_data_frame
)
output_item$factors=mw_output_item(
    name='factors',
    fields=c('study_id','local_sample_id', 'subject_type','factors'),
    inputs=c('study_id','study_title','institute','last_name'),
    match='partial',
    parse_fcn=parse_factors
)

output_item$analysis=mw_output_item(
    name='analysis',
    fields=c('study_id','analysis_id','analysis_summary','analysis_type',
        'ms_instrument_name','ms_instrument_type','ms_type',
        'ion_mode','nmr_instrument_type','nmr_experiment_type',
        'nmr_spectrometer_frequency','nmr_solvent'),
    inputs=c('study_id','study_title','institute','last_name'),
    match='partial',
    parse_fcn=parse_data_frame
)

output_item$metabolites=mw_output_item(
    name='metabolites',
    fields=c('study_id','analysis_id', 'analysis_summary','metabolite_name',
        'refmet_name' ,'pubchem_id','other_id','other_id_type'),
    inputs=c('study_id','analysis_id'),
    match='exact',
    parse_fcn=parse_data_frame
)

output_item$species=mw_output_item(
    name='species',
    fields=c('study_id','latin_name','common_name'),
    inputs=c('study_id','study_title','institute','last_name'),
    match='partial',
    parse_fcn=parse_data_frame
)

output_item$source=mw_output_item(
    name='source',
    fields=c('study_id','sample_source'),
    inputs=c('study_id','study_title','institute','last_name'),
    match='partial',
    parse_fcn=parse_data_frame
)

output_item$disease=mw_output_item(
    name='disease',
    fields=c('study_id','disease'),
    inputs=c('study_id','study_title','institute','last_name'),
    match='partial',
    parse_fcn=parse_data_frame
)

output_item$untarg_studies=mw_output_item(
    name='untarg_studies',
    fields=c('study_id','analysis_id','analysis_display', 'study_title', 'subject_species', 'institute'),
    inputs=c('study_id'),
    match='partial',
    parse_fcn=parse_data_frame
)

output_item$untarg_factors=mw_output_item(
    name='untarg_factors',
    fields=c(''),
    inputs=c('analysis_id'),
    match='exact',
    parse_fcn=parse_untarg_factors
)

output_item$data=mw_output_item(
    name='data',
    fields=c('study_id','analysis_id','analysis_summary','metabolite_name','metabolite_id','refmet_name','units','data'),
    inputs=c('study_id'),
    match='exact',
    parse_fcn=parse_data
)

output_item$mwtab=mw_output_item(
    name='mwtab',
    fields=c(''),
    inputs=c('study_id','analysis_id'),
    match='exact',
    parse_fcn=parse_do_nothing
)

output_item$untarg_data=mw_output_item(
    name='untarg_data',
    fields=c(''),
    inputs=c('analysis_id'),
    match='exact',
    parse_fcn=parse_untarg_data
)

output_item$datatable=mw_output_item(
    name='datatable',
    fields=c(''),
    inputs=c('analysis_id'),
    match='exact',
    parse_fcn=parse_datatable
)

output_item$DatasetExperiment=mw_DE_item(
    name='DatasetExperiment',
    fields='',
    inputs=c('study_id','analysis_id'),
    match='exact',
    parse_fcn=function(response,output_item,input_value){}
)

output_item$untarg_DatasetExperiment=mw_untarg_DE_item(
    name='untarg_DatasetExperiment',
    fields='',
    inputs=c('analysis_id'),
    match='exact',
    parse_fcn=function(response,output_item,input_value){}
)

output_item$MultiAssayExperiment=mw_MAE_item(
    name='MultiAssayExperiment',
    fields='',
    inputs=c('study_id'),
    match='exact',
    parse_fcn=function(response,output_item,input_value){}
)

output_item$SummarizedExperiment=mw_SE_item(
    name='SummarizedExperiment',
    fields='',
    inputs=c('study_id','analysis_id'),
    match='exact',
    parse_fcn=function(response,output_item,input_value){}
)

output_item$untarg_SummarizedExperiment=mw_untarg_SE_item(
    name='untarg_SummarizedExperiment',
    fields='',
    inputs=c('analysis_id'),
    match='exact',
    parse_fcn=function(response,output_item,input_value){}
)
###################### COMPOUND CONTEXT #####################################
input_item$regno = mw_input_item(
    name = 'regno',
    pattern = list(
        exact='^[0-9]{1,12}?$',
        partial='^[0-9]{1,12}?$')
)

input_item$formula = mw_input_item(
    name = 'formula',
    pattern = list(
        exact='^[0-9a-zA-Z]+$',
        partial='^[0-9a-zA-Z]+$')
)
input_item$inchi_key = mw_input_item(
    name = 'inchi_key',
    pattern = list(
        exact='^[A-Z]{14}-[A-Z]{10}-[A-Z]{1}$',
        partial='^([A-Z]{14}\\-[A-Z]{10}\\-[A-Z]?|[A-Z]{14}\\-[A-Z]{0,10}|[A-Z]{0,14})$')
)
input_item$lm_id = mw_input_item(
    name = 'lm_id',
    pattern = list(
        exact='^(LM((FA)|(GL)|(GP)|(SP)|(ST)|(PR)|(SL)|(PK))[0-9]{8,10})$',
        partial='^(L(M[A-Z]{0,2}[0-9]{0,10})?)$')
)
input_item$pubchem_cid = mw_input_item(
    name = 'pubchem_cid',
    pattern = list(
        exact='^[1-9][0-9]{0,8}$',
        partial='^([1-9][0-9]{0,8})?$')
)
input_item$hmdb_id = mw_input_item(
    name = 'hmdb_id',
    pattern = list(
        exact='^HMDB[0-9]+$',
        partial='^H(M(D(B([0-9]+)?)?)?)$')
)
input_item$kegg_id = mw_input_item(
    name = 'kegg_id',
    pattern = list(
        exact='^C[0-9]{5}$',
        partial='^C([0-9]{0,5})?$')
)
input_item$chebi_id = mw_input_item(
    name = 'chebi_id',
    pattern = list(
        exact='^[0-9]+$',
        partial='^([0-9]+)?$')
)
input_item$metacyc_id = mw_input_item(
    name = 'metacyc_id',
    pattern = list(
        exact='^CPD\\-[0-9]+$',
        partial='^C(P(D(\\-([0-9]+)?)?)?)?+$')
)
input_item$abbrev = mw_input_item(
    name = 'abbrev',
    pattern = list(
        exact='^[0-9a-zA-Z]+$',
        partial='^[0-9a-zA-Z]+$')
)
input_item$smiles = mw_input_item(
    name = 'smiles',
    pattern = list(
        exact='^[0-9a-zA-Z]+$',
        partial='^[0-9a-zA-Z]+$')
)
output_item$compound_exact=mw_output_item(
    name='all',
    fields=c('regno','formula','exactmass','inchi_key','name',
    'sys_name','smiles','lm_id','pubchem_cid','hmdb_id','kegg_id',
    'chebi_id','metacyc_id'),
    inputs=c('regno','formula','inchi_key','smiles','lm_id','pubchem_cid','hmdb_id','kegg_id',
        'chebi_id','metacyc_id'),
    match='exact',
    parse_fcn=parse_data_frame
)
output_item$classification=mw_output_item(
    name='classification',
    fields=c('regno','name','sys_name',
        'cf_superclass','cf_class','cf_subclass',
        'cf_direct_parent','cf_alternative_parents',
        'lm_category','lm_main_class',
        'lm_sub_class','lm_class_level4'),
    inputs=c('regno','formula','inchi_key','smiles','lm_id','pubchem_cid','hmdb_id','kegg_id',
        'chebi_id','metacyc_id'),
    match='exact',
    parse_fcn=parse_data_frame
)

output_item$molfile=mw_output_item(
    name='molfile',
    fields=c(''),
    inputs=c('regno','formula','inchi_key','smiles','lm_id','pubchem_cid','hmdb_id','kegg_id',
        'chebi_id','metacyc_id'),
    match='exact',
    parse_fcn=parse_do_nothing
)

output_item$png=mw_output_item(
    name='png',
    fields=c(''),
    inputs=c('regno'),
    match='exact',
    parse_fcn=parse_do_nothing
)

###################### PROTEIN CONTEXT #####################################
input_item$mgp_id = mw_input_item(
    name = 'mgp_id',
    pattern = list(
        exact='^MGP[0-9]{6}$',
        partial='^M(G(P([0-9]{0,6})?)?)?$')
)

input_item$gene_id = mw_input_item(
    name = 'gene_id',
    pattern = list(
        exact='^[0-9]+$',
        partial='^[0-9]+$')
)

input_item$gene_name = mw_input_item(
    name = 'gene_name',
    pattern = list(
        exact='*',
        partial='*')
)

input_item$gene_symbol = mw_input_item(
    name = 'gene_symbol',
    pattern = list(
        exact='*',
        partial='*')
)

input_item$taxid = mw_input_item(
    name = 'taxid',
    pattern = list(
        exact='^[0-9]+$',
        partial='^[0-9]+$')
)

input_item$mrna_id = mw_input_item(
    name = 'mrna_id',
    pattern = list(
        exact='^NM_[0-9]+$',
        partial='^N(M(_([0-9]+)?)?)?$')
)

input_item$refseq_id = mw_input_item(
    name = 'refseq_id',
    pattern = list(
        exact='^NP_[0-9]+$',
        partial='^N(P(_([0-9]+)?)?)?$')
)

input_item$protein_gi = mw_input_item(
    name = 'protein_gi',
    pattern = list(
        exact='^[0-9]+$',
        partial='^[0-9]+$')
)

input_item$uniprot_id = mw_input_item(
    name = 'uniprot_id',
    pattern = list(
        exact='^[A-Z][0-9]+$',
        partial='^([A-Z]([0-9]+)?)?$')
)

input_item$protein_entry = mw_input_item(
    name = 'protein_entry',
    pattern = list(
        exact='*',
        partial='*')
)

input_item$protein_name = mw_input_item(
    name = 'protein_name',
    pattern = list(
        exact='*',
        partial='*')
)

output_item$protein_exact=mw_output_item(
    name='all',
    fields=c('mgp_id','gene_id','gene_name','gene_symbol','taxid','species',
        'species_long','mrna_id','refseq_id',
        'protein_gi','uniprot_id','protein_entry','protein_name','seqlength',
        'seq','is_identical_to'),
    inputs=c('mgp_id','gene_id','gene_name','gene_symbol','taxid','mrna_id',
    'refseq_id','protein_gi','uniprot_id','protein_entry','protein_name'),
    match='exact',
    parse_fcn=parse_data_frame
)

output_item$protein_partial=mw_output_item(
    name='all',
    fields=c('mgp_id','gene_id','gene_name','gene_symbol','taxid','species',
        'species_long','mrna_id','refseq_id',
        'protein_gi','uniprot_id','protein_entry','protein_name','seqlength',
        'seq','is_identical_to'),
    inputs=c('gene_name','protein_name'),
    match='partial',
    parse_fcn=parse_data_frame
)

###################################### GENE ###################################
# inputs already specified in protein context

output_item$gene_exact=mw_output_item(
    name='all',
    fields=c('mgp_id','gene_id','gene_name','gene_symbol',
        'gene_synonyms','alt_names','chromosome',
        'map_location','summary','taxid',
        'species','species_long'),
    inputs=c('mgp_id','gene_id','gene_name','gene_symbol','taxid'),
    match='exact',
    parse_fcn=parse_data_frame
)

output_item$gene_partial=mw_output_item(
    name='all',
    fields=c('mgp_id','gene_id','gene_name','gene_symbol',
        'gene_synonyms','alt_names','chromosome',
        'map_location','summary','taxid',
        'species','species_long'),
    inputs=c('gene_name'),
    match='partial',
    parse_fcn=parse_data_frame
)

###################################### REFMET ###################################
# some inputs already specified in other contexts
input_item$name = mw_input_item(
    name = 'name',
    pattern = list(
        exact='*',
        partial='*')
)
input_item$main_class = mw_input_item(
    name = 'main_class',
    pattern = list(
        exact='*',
        partial='*')
)
input_item$sub_class = mw_input_item(
    name = 'sub_class',
    pattern = list(
        exact='*',
        partial='*')
)
input_item$super_class = mw_input_item(
    name = 'super_class',
    pattern = list(
        exact='*',
        partial='*')
)

input_item$match = mw_input_item(
    name = 'match',
    pattern = list(
        exact='*',
        partial='*')
)

output_item$refmet_exact=mw_output_item(
    name='all',
    fields=c('name','sys_name','synonyms',
        'pubchem_cid','inchi_key',
        'exactmass','formula',
        'main_class','sub_class','super_class'),
    inputs=c('name','pubchem_cid',
            'inchi_key','formula',
            'main_class','sub_class','super_class'),
    match='exact',
    parse_fcn=parse_data_frame
)

###### moverz
input_item$database = mw_input_item(
    name = 'database',
    pattern = list(
        exact = '^((LIPIDS)|(REFMET)|(MB))$',
        partial = '^((LIPIDS)|(REFMET)|(MB))$'
        )
)
input_item$mz = mw_input_item(
    name = 'mz',
    pattern = list(
        exact = '^[0-9]\\d*(\\.\\d+)?$',
        partial = '^[0-9]\\d*(\\.\\d+)?$'
    )
)
input_item$ion = mw_input_item(
    name = 'ion',
    pattern = list(
        exact = '*',
        partial = '*'
    )
)
input_item$tolerance = mw_input_item(
    name = 'tolerance',
    pattern = list(
        exact = '^[0-9]\\d*(\\.\\d+)?$',
        partial = '^[0-9]\\d*(\\.\\d+)?$'
    )
)
input_item$moverz=list(
    input_item$database,
    input_item$mz,
    input_item$ion,
    input_item$tolerance
)
output_item$moverz=mw_output_item(
    name='moverz',
    fields=c("input_mz","matched_mz","delta","name","systematic_name","formula",
        "ion","category","main_class","sub_class"),
    inputs=c('database','mz','ion','tolerance'),
    match='exact',
    parse_fcn=parse_moverz
)

###### exactmass

input_item$lipid = mw_input_item(
    name = 'lipid',
    pattern = list(
        exact = '*',
        partial = '*'
    )
)

input_item$exactmass=list(
    input_item$lipid,
    input_item$ion
)

output_item$exactmass=mw_output_item(
    name='exactmass',
    fields=c("input_abbrev", "input_ion_type", "exact_mass", "molecular_formula"),
    inputs=c('lipid','ion'),
    match='exact',
    parse_fcn=parse_exactmass
)




