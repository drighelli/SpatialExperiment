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
    expect_null(imgData(spe))
    expect_true(isEmpty(spatialData(spe)))
    expect_identical(colData(spe), colData(sce))
    
    spe <- .sce_to_spe(sce, imgData=id)
    expect_identical(imgData(spe), id)
})

test_that("scaleFactors should be numeric, a named list or JSON file", {
    sce <- SingleCellExperiment(assays=list(counts=y), colData=cd)
    dir <- system.file(
        file.path("extdata", "10xVisium", "section1", "spatial"),
        package = "SpatialExperiment")
    sf_fn <- file.path(dir, "scalefactors_json.json")
    img_fn <- file.path(dir, "tissue_lowres_image.png")
    
    expect_error(.sce_to_spe(sce, imageSources=img_fn, scaleFactors="x"))
    expect_error(.sce_to_spe(sce, imageSources=img_fn, scaleFactors=list()))
    
    expect_silent(.sce_to_spe(sce, imageSources=img_fn, scaleFactors=1))
    expect_silent(.sce_to_spe(sce, imageSources=img_fn, scaleFactors=sf_fn))
    expect_silent(.sce_to_spe(sce, imageSources=img_fn, 
        scaleFactors=list(tissue_lowres_scalef=1)))
})
