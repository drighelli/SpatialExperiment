example(SpatialExperiment, echo = FALSE)

test_that("empty constructor", {
    spe <- SpatialExperiment()
    expect_is(imgData(spe), "DFrame")
    expect_true(isEmpty(imgData(spe)))
    expect_identical(spatialDataNames(spe), character())
    expect_is(spatialData(spe), "DFrame")
    expect_true(isEmpty(spatialData(spe)))
    expect_equal(dim(spatialData(spe)), c(ncol(spe), 0))
    expect_null(spatialCoordsNames(spe))
    expect_is(spatialCoords(spe), "matrix")
    expect_equal(dim(spatialCoords(spe)), c(ncol(spe), 0))
})

test_that("spatialCoordsNames = character in colData", {
    cd <- DataFrame(x=numeric(), y=numeric())
    spe <- SpatialExperiment(colData=cd, spatialCoordsNames=names(cd))
    expect_identical(spatialCoordsNames(spe), names(cd))
    expect_identical(spatialCoords(spe), as.matrix(cd))  
})

test_that("spatialDataNames = character in colData", {
    cd <- DataFrame(x=numeric(), y=numeric())
    spe <- SpatialExperiment(colData=cd, spatialDataNames=names(cd))
    expect_identical(spatialDataNames(spe), names(cd))
    expect_identical(spatialData(spe), cd)
})

test_that("spatialData/CoordsNames = character in colData", {
    cd <- DataFrame(x=numeric(), y=numeric(), z=numeric())
    spe <- SpatialExperiment(
        colData=cd, 
        spatialDataNames="z",
        spatialCoordsNames=c("x", "y"))
    expect_identical(spatialDataNames(spe), "z")
    expect_identical(spatialCoordsNames(spe), c("x", "y"))
    expect_true(!any(c("x", "y") %in% names(colData(spe))))
})

test_that("spatialCoords = numeric matrix", {
    y <- diag(n <- 10)
    mat <- matrix(0, n, m <- 2)
    spe <- SpatialExperiment(assays = y, spatialCoords = mat)
    expect_is(spatialCoords(spe), "matrix")
    expect_identical(spatialCoords(spe), mat)
    expect_null(spatialCoordsNames(spe))
    
    colnames(mat) <- seq(m)
    spe <- SpatialExperiment(assays = y, spatialCoords = mat)
    expect_is(spatialCoords(spe), "matrix")
    expect_identical(spatialCoords(spe), mat)
    expect_identical(spatialCoordsNames(spe), colnames(mat))
})

test_that("spatialData = DFrame", {
    mat <- matrix(0, n <- 10, m <- 5)
    expect_error(SpatialExperiment(spatialData = mat))
    y <- diag(n)
    df <- DataFrame(mat)
    names(df) <- letters[seq(m)]
    spe <- SpatialExperiment(assays = y, spatialData = df)
    expect_is(spatialData(spe), "DFrame")
    expect_identical(spatialData(spe), df)
    expect_identical(spatialDataNames(spe), names(df))
})

test_that("message when spatialData & -Names are supplied", {
    cd <- DataFrame(x=numeric(), y=numeric())
    expect_message(
        spe <- SpatialExperiment(
            colData=cd, 
            spatialData=cd, 
            spatialDataNames=names(cd)))
    expect_identical(spe, 
        SpatialExperiment(
            colData=cd, 
            spatialDataNames=names(cd)))
})

test_that("message when spatialCoords & -Names are supplied", {
    cd <- DataFrame(x=numeric(), y=numeric())
    expect_message(
        spe <- SpatialExperiment(
            colData=cd, 
            spatialCoords=cd, 
            spatialCoordsNames=names(cd)))
    expect_identical(spe, 
        SpatialExperiment(
            colData=cd, 
            spatialCoordsNames=names(cd)))
})

gs <- paste0("gene", seq_len(ng <- 200))
cs <- paste0("cell", seq_len(nc <- 100))
y <- matrix(runif(ng*nc), ng, nc, dimnames=list(gs, cs))

sample_id <- paste0("sample", c(1, 2))
cd <- DataFrame(sample_id=sample(sample_id, nc, TRUE))

image_id <- paste0("image", c(1, 2))
spi <- SpatialImage(as.raster(0))
id <- DataFrame(sample_id, image_id, data=I(list(spi)), scaleFactor=1)

test_that("input sample_ids are used if is.null(x$sample_id)", {
    sce <- SingleCellExperiment(assays=list(counts=y))
    spe <- .sce_to_spe(sce, sample_id="foo")
    expect_true(all(spe$sample_id == "foo"))
})

test_that("input sample_id is ignored if !is.null(x$sample_id)", {
    sce <- SingleCellExperiment(assays=list(counts=y), colData=cd)
    spe <- .sce_to_spe(sce, sample_id="foo")
    expect_true(all(spe$sample_id == sce$sample_id))
})

test_that("'sample_id's must be character of length 1 or ncol(.)", {
    sce <- SingleCellExperiment(assays=list(counts=y))
    spe <- .sce_to_spe(sce, sample_id=cd$sample_id)
    expect_identical(spe$sample_id, cd$sample_id)
    expect_error(.sce_to_spe(sce, sample_id=1))
    expect_error(.sce_to_spe(sce, sample_id=c("a", "b")))
})

test_that(".sce_to_spe()", {
    sce <- SingleCellExperiment(
        assays=list(counts=y),
        colData=cd)
    spe <- .sce_to_spe(sce)
    expect_is(spe, "SpatialExperiment")
    expect_is(imgData(spe), "DFrame")
    expect_true(isEmpty(imgData(spe)))
    expect_is(spatialData(spe), "DFrame")
    expect_true(isEmpty(spatialData(spe)))
    expect_identical(colData(spe), colData(sce))
    
    spe <- .sce_to_spe(sce, imgData=id)
    expect_identical(imgData(spe), id)
})

test_that("scaleFactors should be numeric, a named list or JSON file", {
    sce <- SingleCellExperiment(assays=list(counts=y), colData=cd)
    dir <- system.file(
        file.path("extdata", "10xVisium", "section1", "outs", "spatial"),
        package="SpatialExperiment")
    sf_fn <- file.path(dir, "scalefactors_json.json")
    img_fn <- file.path(dir, "tissue_lowres_image.png")
    
    expect_error(.sce_to_spe(sce, imageSources=img_fn, scaleFactors="x"))
    expect_error(.sce_to_spe(sce, imageSources=img_fn, scaleFactors=list()))
    
    expect_silent(.sce_to_spe(sce, imageSources=img_fn, scaleFactors=1))
    expect_silent(.sce_to_spe(sce, imageSources=img_fn, scaleFactors=sf_fn))
    expect_silent(.sce_to_spe(sce, imageSources=img_fn, 
        scaleFactors=list(tissue_lowres_scalef=1)))
})

test_that("deprecated spatialData/Names returns message", {
    expect_message(spatialData(spe))
    expect_message(spatialDataNames(spe))
    expect_message(SpatialExperiment(spatialData = DataFrame()))
    cd <- DataFrame(x = numeric(), y = numeric())
    expect_message(SpatialExperiment(colData = cd, spatialDataNames = names(cd)))
})

test_that("Additional arguments for the constructor of SpatialExperiment", {
  img <- system.file(
    "extdata", "10xVisium", "section1", "outs", "spatial", 
    "tissue_lowres_image.png", package="SpatialExperiment")
  
  # New columns named by any of "assays, rowData, rowRanges, colData,
  # metadata, checkDimnames" do not go to imgData.
  spe_1 <- SpatialExperiment(
    assays=diag(n <- 10),
    rowRanges=GRanges(rep("chr1", 10), IRanges(1, 100)),
    colData=DataFrame(a=seq(n)),
    metadata=list(),
    checkDimnames=FALSE,
    sample_id="foo",
    imageSources=c(img, img),
    image_id=c("bar_1", "bar_2"))
  expect_false(any(
    c("assays", "rowRanges", "colData", "metadata", "checkDimnames") %in% colnames(imgData(spe_1))
  ))
  
  spe_2 <- SpatialExperiment(
    assays=diag(n <- 10),
    rowData=DataFrame(a=seq(n)),
    colData=DataFrame(b=seq(n)),
    metadata=list(),
    checkDimnames=FALSE,
    sample_id="foo",
    imageSources=c(img, img),
    image_id=c("bar_1", "bar_2"),
    my_col = c("foo_1", "foo_2"))
  expect_false(any(
    c("assays", "rowData", "colData", "metadata", "checkDimnames") %in% colnames(imgData(spe_2))
  ))
  expect_true("my_col" %in% colnames(imgData(spe_2)))
})

