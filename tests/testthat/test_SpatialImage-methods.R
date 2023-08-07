# dim ----

test_that("dim,Stored-/LoadedSpatialImage is instant", {
    src <- system.file(
        "extdata", "10xVisium", "section1", "outs", "spatial", 
        "tissue_lowres_image.png", package="SpatialExperiment")
    spi <- SpatialImage(src)
    src <- normalizePath(src)
    # dim,VirtualSPI affects cache
    selectMethod("dim", "VirtualSpatialImage")(spi)
    expect_true(!is.null(image.cache$cached[[src]]))
    # dim,StoredSPI does not
    .flush_cache(); dim(spi)
    expect_true(is.null(image.cache$cached[[src]]))
})

# getters ----
url <- "https://i.redd.it/3pw5uah7xo041.jpg"
spi <- new("RemoteSpatialImage", url=url)
    
test_that("imgSource returns NA for LoadedSpI (irrespective of 'path')", {
    x <- as(spi, "LoadedSpatialImage")
    expect_true(is.na(imgSource(x, path=FALSE)))
    expect_true(is.na(imgSource(x, path=TRUE)))
})

test_that("imgSource,path=FALSE/TRUE returns URL/path for RemoteSpI", {
    expect_identical(imgSource(spi, path=FALSE), url)
    x <- imgSource(spi, path=TRUE)
    expect_is(x, "character")
    expect_true(file.exists(x))
})

test_that("imgSource returns path for StoredSpI (irrespective of 'path')", {
    x <- as(spi, "StoredSpatialImage")
    expect_identical(
        imgSource(x, path=FALSE),
        imgSource(x, path=TRUE))
})

# utils ----
.mockRaster <- function(n, m) {
    x <- runif(n*m)
    y <- matrix(x, n, m)
    as.raster(y)
}
.mockSPI <- function(n, m) {
    r <- .mockRaster(n, m)
    new("LoadedSpatialImage", image=r)
}
.mockSPE <- function(n, m, N) {
    l <- replicate(N, .mockSPI(n, m))
    sample_id <- paste0("sample", seq_len(N))
    image_id <- paste0("image", seq_len(N))
    cd <- DataFrame(sample_id)
    df <- DataFrame(sample_id, image_id, data=I(l), scaleFactor=seq_len(N))
    SpatialExperiment(colData=cd, imgData=df)
}

# params----
n <- 10 # number of rows / height
m <- 20 # number of columns / width
N <- 3  # number of images in SpE

# rotate----
test_that(".rotate/rotateImg(),degrees=0 does nothing", {
    # raster as input
    r <- .mockRaster(n, m)
    s <- .rotate(r, 0)
    expect_identical(r, s)
    # SpI as input
    i <- .mockSPI(n, m)
    j <- rotateImg(i, 0)
    expect_identical(i, j)
    # SpE as input
    x <- .mockSPE(n, m, N)
    y <- rotateImg(x, TRUE, TRUE, 0)
    expect_identical(x, y)
    
})
test_that(".rotate/rotateImg(),degrees>0 rotates right/clockwise", {
    .test <- function(x, y) {
        expect_identical(dim(x), rev(dim(y)))
        expect_identical(c(x[1, ]), c(y[, n]))
    }
    # raster as input
    r <- .mockRaster(n, m)
    s <- .rotate(r, 90)
    .test(r, s)
    # SpI as input
    i <- .mockSPI(n, m)
    j <- rotateImg(i, 90)
    .test(imgRaster(i), imgRaster(j))
    # SpE as input
    # first only
    sid <- iid <- NULL
    x <- .mockSPE(n, m, N)
    y <- rotateImg(x, sid, iid, 90)
    .test(imgRaster(x, sid, iid), imgRaster(y))
    expect_identical(
        getImg(x, TRUE, TRUE)[-1],
        getImg(y, TRUE, TRUE)[-1])
    # all images
    sid <- iid <- TRUE
    x <- .mockSPE(n, m, N)
    y <- rotateImg(x, sid, iid, 90)
    mapply(
        function(u, v) .test(u, v),
        u = imgRaster(x, sid, iid),
        v = imgRaster(y, sid, iid))
})
test_that(".rotate/.rotateImg(),degrees<0 rotates left/counter-clockwise", {
    .test <- function(x, y) {
        expect_identical(dim(x), rev(dim(y)))
        expect_identical(c(x[1, ]), rev(y[, 1]))
    }
    # raster as input
    r <- .mockRaster(n, m)
    s <- .rotate(r, -90)
    .test(r, s)
    # SpI as input
    i <- .mockSPI(n, m)
    j <- rotateImg(i, -90)
    .test(imgRaster(i), imgRaster(j))
    # first only
    sid <- iid <- NULL
    x <- .mockSPE(n, m, N)
    y <- rotateImg(x, sid, iid, -90)
    .test(imgRaster(x, sid, iid), imgRaster(y))
    expect_identical(
        getImg(x, TRUE, TRUE)[-1],
        getImg(y, TRUE, TRUE)[-1])
    # all images
    sid <- iid <- TRUE
    x <- .mockSPE(n, m, N)
    y <- rotateImg(x, sid, iid, -90)
    mapply(
        function(u, v) .test(u, v),
        u = imgRaster(x, sid, iid),
        v = imgRaster(y, sid, iid))
})
test_that("rotateImg() w/o imgData passes", {
    x <- .mockSPE(n, m, N)
    imgData(x) <- NULL
    expect_silent(y <- rotateImg(x))
    expect_identical(x, y)
})

# mirror ----
test_that(".mirror/mirrorImg(),axis='h' flips horizontally", {
    .test <- function(x, y) {
        expect_identical(dim(x), dim(y))
        expect_identical(c(x[1, ]), c(y[n, ]))
    }
    # raster as input
    r <- .mockRaster(n, m)
    s <- .mirror(r, "h")
    .test(r, s)
    # SpI as input
    i <- .mockSPI(n, m)
    j <- mirrorImg(i, "h")
    .test(imgRaster(i), imgRaster(j))
    # SpE as input
    # first only
    sid <- iid <- NULL
    x <- .mockSPE(n, m, N)
    y <- mirrorImg(x, sid, iid, "h")
    .test(imgRaster(x, sid, iid), imgRaster(y))
    expect_identical(
        getImg(x, TRUE, TRUE)[-1],
        getImg(y, TRUE, TRUE)[-1])
    # all images
    sid <- iid <- TRUE
    x <- .mockSPE(n, m, N)
    y <- mirrorImg(x, sid, iid, "h")
    mapply(
        function(u, v) .test(u, v),
        u = imgRaster(x, sid, iid),
        v = imgRaster(y, sid, iid))
})
test_that(".mirror/mirrorImg(),axis='v' flips vertically", {
    .test <- function(x, y) {
        expect_identical(dim(x), dim(x))
        expect_identical(c(x[1, ]), rev(y[1, ]))
    }
    # raster as input
    r <- .mockRaster(n, m)
    s <- .mirror(r, "v")
    .test(r, s)
    # SpI as input
    i <- .mockSPI(n, m)
    j <- mirrorImg(i, "v")
    .test(imgRaster(i), imgRaster(j))
    # SpE as input
    # first only
    sid <- iid <- NULL
    x <- .mockSPE(n, m, N)
    y <- mirrorImg(x, sid, iid, "v")
    .test(imgRaster(x, sid, iid), imgRaster(y))
    expect_identical(
        getImg(x, TRUE, TRUE)[-1],
        getImg(y, TRUE, TRUE)[-1])
    # all images
    sid <- iid <- TRUE
    x <- .mockSPE(n, m, N)
    y <- mirrorImg(x, sid, iid, "v")
    mapply(
        function(u, v) .test(u, v),
        u = imgRaster(x, sid, iid),
        v = imgRaster(y, sid, iid))
})
test_that("mirrorImg() w/o imgData passes", {
    x <- .mockSPE(n, m, N)
    imgData(x) <- NULL
    expect_silent(y <- mirrorImg(x))
    expect_identical(x, y)
})
