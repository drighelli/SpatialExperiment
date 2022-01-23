# SpatialExperiment

`SpatialExperiment` is an R/Bioconductor S4 class for storing data from spatially resolved transcriptomics (ST) experiments. The class extends the `SingleCellExperiment` class for single-cell data to support storage and retrieval of additional information from spot-based and molecule-based ST platforms, including spatial coordinates, images, and image metadata. A specialized constructor function is included for data from the 10x Genomics Visium platform.

The `SpatialExperiment` package is available from [Bioconductor](https://bioconductor.org/packages/SpatialExperiment).

A vignette containing examples and documentation is available from [Bioconductor](https://bioconductor.org/packages/SpatialExperiment), and additional details are provided in our [preprint](https://www.biorxiv.org/content/10.1101/2021.01.27.428431v2).


## Installation

The `SpatialExperiment` package can be installed from Bioconductor. Note that Bioconductor follows a "release" and "development" schedule, where the release version is considered to be stable and updated every 6 months, and the development version contains latest updates (and then becomes the next release version every 6 months).


### Release version

To install the stable release version, install the latest release version of R from [CRAN](https://cran.r-project.org/), then install the Bioconductor package installer and the `SpatialExperiment` package as follows.

```
install.packages("BiocManager")
BiocManager::install("SpatialExperiment")
```

### Development version

To install the development version, there are two options.

(i) Install the [appropriate version of R (R-release between April and October, or R-devel between October and April)](http://bioconductor.org/developers/how-to/useDevel/), then install the Bioconductor package installer and the development version of the `SpatialExperiment` package as follows.

```
install.packages("BiocManager")
BiocManager::install("SpatialExperiment", version = "devel")
```

(ii) Alternatively, if you do not want to install R-devel (since this may cause issues with other packages), you can use the latest release version of R from [CRAN](https://cran.r-project.org/) and install the development version of the `SpatialExperiment` package from GitHub as follows.

```
install.packages("remotes")
remotes::install_github("drighelli/SpatialExperiment")
```

### Updates in version 1.5.2 (2022-01-09)

In version 1.5.2 (2022-01-09) the `spatialData` slot was deprecated, with columns previously stored in `spatialData` now recommended to be stored in `colData`. The `spatialCoords` slot (`x` and `y` coordinates) is unchanged. Existing objects are backward-compatible (and a console message is returned if `spatialData` is still used), however we recommend re-building existing objects to move the contents of `spatialData` to `colData` to ensure longer-term compatibility with any future updates.

**To use version 1.5.2 or later (as of January 2022), please follow the installation instructions for the development version above.**

(These updates will also be available in the release version from mid-April 2022 onwards, following the Bioconductor release schedule.)


## Citation

Righell D.\*, Weber L.M.\*, Crowell H.L.\*, Pardo B., Collado-Torres L., Ghazanfar S., Lun A.T.L., Hicks S.C.<sup>+</sup>, and Risso D.<sup>+</sup> (2021), *SpatialExperiment: infrastructure for spatially resolved transcriptomics data in R using Bioconductor*, [bioRxiv](https://www.biorxiv.org/content/10.1101/2021.01.27.428431v2).

