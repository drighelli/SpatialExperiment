# SpatialExperiment

SpatialExperiment package provides an S4 class for Spatial Omics data handling.

Since version 1.1.42 SpatialExperiment package has changed in several ways.

+ VisiumExperiment and SpatialExperiment classes are merged into the
SpatialExperiment class.
+ Newer version of SpatialExperiment has image loading handling with `imgData` 
structure
+ Molecular-based data can be handled with `molecular` [BumpyMatrix](http://bioconductor.org/packages/BumpyMatrix/) assay.
+ 10x Visium standard data import support with `read10xVisium` function.
+ Check vignettes and documentation for further details.


# Installation

SpatialExperiment is available on the [Bioductor official repository](https://bioconductor.org/packages/SpatialExperiment/).
Today (January 2021) version 1.1.42 is available on Bioconductor devel (v3.13).

To install it use:

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# The following initializes usage of Bioc devel
BiocManager::install(version='devel')

BiocManager::install("SpatialExperiment")
```

or, alternatively, use the following for the latest github-devel version:

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("drighelli/SpatialExperiment")
```

# Add-ons

## Spatial Experiment and multimodal data 

SpatialExperiment class can be included into a MultiAssayExperiment object as 
a dedicated assay.
Check how it handles seqFISH data into the `SingleCellMultiModal::seqFISH()`
package function.

SingleCellMultiModal is available in Bioconductor and can be installed via:

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("SingleCellMultiModal")
```


## Spatial Analysis

To provide additional examples for spatial transcriptomics analysis we're constantly populating the https://github.com/drighelli/SpatialAnalysisWorkflows repository.

These will hopefully find a place into a dedicated package for Spatial Transcriptomics data analysis examples.


