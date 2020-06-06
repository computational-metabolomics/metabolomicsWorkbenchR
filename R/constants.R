#' @include generics.R class_def.R

#' @export
output_item = list()

#' @export
input_item = list()

#' @export
context = list()

################################## CONTEXTS ###################################

context$study = mw_context(
    name = 'study',
    input_items = c('study_id','study_title','institute','last_name','analysis_id','metabolite_id','ignored'),
    output_items = c('summary','factors','analysis','metabolites','mwtab','source','species','disease','number_of_metabolites','data','datatable','untarg_studies','untarg_factors','untarg_data','metabolite_info'),
    allow = 'one'
)

context$compound = mw_context(
    name = 'compound',
    input_items = c('regno','formula','inchi_key','lm_id','pubchem_cid','hmdb_id','kegg_id','chebi_id','metacyc_id','abbrev'),
    output_items = c('regno','formula','exactmass','inchi_key','name','sys_name','smiles','lm_id','pubchem_cid','hmdb_id','kegg_id','chebi_id','metacyc_id','classification','molfile','png'),
    allow = 'any'
)    

context$refmet = mw_context(
    name = 'refmet',
    input_items = c('match','name','inchi_key','regno','pubchem_cid','formula','main_class','sub_class'),
    output_items = c('name','inchi_key','regno','pubchem_cid','exactmass','formula','synonyms','sys_name','main_class','sub_class'),
    allow = 'any'
)

context$gene = mw_context(
    name = 'gene',
    input_items = c('mgp_id','gene_id','gene_name','gene_symbol','taxid'),
    output_items = c('lmp_id','mgp_id','gene_id','gene_name','gene_symbol','gene_synonyms','alt_names',
        'chromosome','map_location','summary','taxid','species','species_long'),
    allow = 'any'
)

context$protein = mw_context(
    name = 'protein',
    input_items = c('mgp_id','gene_id','gene_name','gene_symbol','taxid','mrna_id','refseq_id',
        'protein_gi','uniprot_id','protein_entry','protein_name'),
    output_items = c('mgp_id','gene_id','gene_name','gene_symbol','taxid','species','species_long',
        'mrna_id','refseq_id','protein_gi','uniprot_id','protein_entry','protein_name','seqlength','seq',
        'is_identical_to'),
    allow = 'any'
)

context$moverz = mw_moverz_context(
    input_items = c('LIPIDS','MB','REFMET'),
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
        partial='^S(T[0-9]{0,6})?$')
)
input_item$analysis_id = mw_input_item(
    name = 'analysis_id',
    pattern = list(
        exact='^AN[0-9]{6}$',
        partial='^A(N[0-9]{0,6})?$')
)
input_item$metabolite_id = mw_input_item(
    name = 'metabolite_id',
    pattern = list(
        exact='^ME[0-9]{6}$',
        partial='^M(E[0-9]{0,6})?$')
)
input_item$study_title = mw_input_item(
    name = 'study_title',
    pattern = list(
        exact='*',
        partial='*')
)
input_item$institute = mw_input_item(
    name = 'institute',
    pattern = list(
        exact='*',
        partial='*')
)
input_item$last_name = mw_input_item(
    name = 'last_name',
    pattern = list(
        exact='*',
        partial='*')
)
input_item$ignored = mw_input_item(
    name = 'study_id',
    pattern = list(
        exact='*',
        partial='*')
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
    parse_fcn=function(x,y){return(x)}
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