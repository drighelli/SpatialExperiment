dir <- system.file(
    file.path("extdata", "10xVisium", "section1"),
    package = "SpatialExperiment")
fnm <- file.path(dir, "spatial", "tissue_positions_list.csv")
xyz <- read.csv(fnm, header = FALSE, col.names = c(
    "barcode", "in_tissue", "array_row", "array_col",
    "pxl_row_in_fullres", "pxl_col_in_fullres"))
img <- readImgData(
    path = file.path(dir, "spatial"),
    sample_id="sample01")

test_that("SingleCellExperiment to SpatialExperiment", {
    example(SingleCellExperiment, echo=FALSE)
    
    sce1 <- sce <- sce[1:50, 1:50]
    int_colData(sce1)$spatialData <- DataFrame(xyz[,c(1:4)])
    int_colData(sce1)$spatialCoords <- as.matrix(xyz[,c(5,6)])
    expect_warning(as(sce1, "SpatialExperiment"))
    int_colData(sce1)$imgData <- img
    
    expect_s4_class(as(sce1, "SpatialExperiment"), class="SpatialExperiment")

    expect_s4_class(toSpatialExperiment(sce, imgData = img,
        spatialData=DataFrame(xyz),
        spatialCoordsNames=c("pxl_col_in_fullres", "pxl_row_in_fullres"),
        sample_id="sample01"), "SpatialExperiment")
})

