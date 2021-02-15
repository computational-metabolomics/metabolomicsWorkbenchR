
<!-- badges: start -->
[![Build Status](https://github.com/computational-metabolomics/structToolbox/workflows/structToolbox/badge.svg)](https://github.com/computational-metabolomics/structToolbox/actions)
[![Codecov test coverage](https://codecov.io/gh/computational-metabolomics/metabolomicsWorkbenchR/branch/master/graph/badge.svg)](https://codecov.io/gh/computational-metabolomics/metabolomicsWorkbenchR?branch=master)
<!-- badges: end -->

# metabolomicsWorkbenchR 
A Bioconductor package for interfacing with the Metabolomics Workbench API (https://www.metabolomicsworkbench.org/).

## Installation
To install this package, start R (version "4.0") and enter:

```{r}
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("metabolomicsWorkbenchR")
```

Alternatively, to install from github use the `remotes` package, where `@master` can be replaced with release tags if desired:
```{r}
remotes::install_github('computational-metabolomics/metabolomicsWorkbenchR@master')
```


## Introduction
This package enables access to the Metabolomics Workbench API (MWA) using a simple query interface. For example, the 'study' context can be queried using the 'study_title' input to search the database for all studies with the keyword 'Diabetes' in the title and return a summary:

```{r}
library(metabolomicsWorkBench)
df = do_query(context = 'study', input_item = 'study_title', input_value = 'Diabetes', output_item = 'summary')
```

The query interface is designed to mirror the API documentation as closely as possible (https://www.metabolomicsworkbench.org/tools/MWRestAPIv1.0.pdf). 

Methods have been included using multiple queries to coerce study data from the database into different data structures used within Bioconductor, namely `SummarizedExperiment` and `MultiAssayExperiment`.

In addition methods have been included to coerce study data into `DatasetExperiment` objects for compatibility with our `struct` and `structToolbox` packages (both available on Bioconductor).

