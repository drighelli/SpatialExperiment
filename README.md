# SpatialExperiment

SpatialExperiment package provides S4 classes for Spatial Transcriptomics data handling.
At the moment it provides SpatialExperiment and VisiumExperiment classes, providing basical setters/getters for spatial sequencing experiments.

SpatialExperiment is now available on the [Bioductor official repository](https://bioconductor.org/packages/SpatialExperiment/).


# Installing devel version of the package

To install the latest devel version please run

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("devtools")
devtools::install_github("drighelli/SpatialExperiment", ref="devel")

```

# Spatial Analysis

To provide additional examples for spatial transcriptomics analysis we're populating the https://github.com/drighelli/SpatialAnalysisWorkflows repository.


