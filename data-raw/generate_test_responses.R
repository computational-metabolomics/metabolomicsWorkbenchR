Q=list(
    "https://www.metabolomicsworkbench.org/rest/study/metabolite_id/ME000096/summary",
    "https://www.metabolomicsworkbench.org/rest/study/metabolite_id/ME000096/summary",
    "https://www.metabolomicsworkbench.org/rest/study/metabolite_id/ME000096/summary",
    "https://www.metabolomicsworkbench.org/rest/study/study_id/ST000001/summary",
    "https://www.metabolomicsworkbench.org/rest/study/study_id/ST000001/number_of_metabolites",
    "https://www.metabolomicsworkbench.org/rest/study/study_id/ST000001/analysis",
    "https://www.metabolomicsworkbench.org/rest/study/study_id/ST000001/metabolites",
    "https://www.metabolomicsworkbench.org/rest/study/study_id/ST000001/species",
    "https://www.metabolomicsworkbench.org/rest/study/study_id/ST000001/source",
    "https://www.metabolomicsworkbench.org/rest/study/study_id/ST000010/disease",
    "https://www.metabolomicsworkbench.org/rest/study/study_id/ST000010/untarg_studies",
    "https://www.metabolomicsworkbench.org/rest/study/analysis_id/AN000023/untarg_factors",
    "https://www.metabolomicsworkbench.org/rest/study/study_id/ST000001/data",
    "https://www.metabolomicsworkbench.org/rest/study/analysis_id/AN000023/untarg_data",
    "https://www.metabolomicsworkbench.org/rest/study/analysis_id/AN000023/datatable",
    "https://www.metabolomicsworkbench.org/rest/compound/regno/11/all",
    "https://www.metabolomicsworkbench.org/rest/compound/regno/11/classification",
    "https://www.metabolomicsworkbench.org/rest/protein/uniprot_id/Q13085/all",
    "https://www.metabolomicsworkbench.org/rest/gene/mgp_id/MGP000016/all",
    "https://www.metabolomicsworkbench.org/rest/refmet/name/Cholesterol/all",
    "https://www.metabolomicsworkbench.org/rest/moverz/MB/635.52/M+H/0.5",
    "https://www.metabolomicsworkbench.org/rest/moverz/LIPIDS/513.45/M-2H/0.2",
    "https://www.metabolomicsworkbench.org/rest/moverz/REFMET/255.2/M+H/0.2",
    "https://www.metabolomicsworkbench.org/rest/exactmass/PC(34:1)/M+H"
)

R=list()

for (k in 1:length(Q)) {
    R[[k]]=httr::GET(url=Q[[k]])
}

usethis::use_data(R,internal=TRUE,overwrite=TRUE)