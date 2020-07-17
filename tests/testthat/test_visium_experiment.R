context("VisiumExperiment object")
test_that("Visium Experiment methods testing", {
    barcodesFile <- system.file(file.path("extdata", "10x_visium",
                          "barcodes.tsv"),
                          package="SpatialExperiment")
    barcodesEx <- read.csv(barcodesFile, sep="\t",
                          header=FALSE, col.names=c("Barcodes"))
    featuresFile <- system.file(file.path("extdata", "10x_visium",
                          "features.tsv"), package="SpatialExperiment")
    featuresEx <- read.csv(featuresFile, sep="\t",
                          header=FALSE, col.names=c("Barcodes", "Feature_name",
                          "Feature_type"))
    countsFile <- system.file(file.path("extdata", "10x_visium",
                          "matrix.mtx"), package="SpatialExperiment")
    countsEx <- Matrix::readMM(file=countsFile)
    posFile <- system.file(file.path("extdata", "10x_visium",
                          "tissue_positions_list.tsv"),
                          package="SpatialExperiment")
    tissPosEx <- read.csv(posFile,
                          sep="\t", header=FALSE,
                          col.names=c("Barcodes", "in_tissue",
                           "array_row", "array_col",
                           "pxl_col_in_fullres", "pxl_row_in_fullres"))
    scaleFile <- system.file(file.path("extdata", "10x_visium",
                                     "scalefactors_json.json"),
                           package="SpatialExperiment")
    
    scalefactors <- rjson::fromJSON(file=scaleFile)
    ve <- VisiumExperiment(rowData=featuresEx, colData=barcodesEx,
                           assays=c(counts=countsEx),
                           spatialCoords=tissPosEx,
                           scaleFactors=scalefactors)
    ## testing scale factors
    expect_identical(scalefactors, scaleFactors(ve))
    
    ## testing visium coordinates
    expect_error(expect_identical(tissPosEx, spatialCoords(ve)))
    expect_identical(DataFrame(tissPosEx), spatialCoords(ve))
})
