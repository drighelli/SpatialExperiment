example(read10xVisium)

test_that("i=TRUE,j=TRUE retains all data", {
    expect_identical(ve, ve[TRUE, TRUE])
})

test_that("i=TRUE,j=FALSE drops all samples and imgData", {
    x <- ve[TRUE, FALSE]
    expect_true(ncol(x) == 0)
    expect_true(nrow(x) == nrow(ve))
    expect_true(isEmpty(imgData(x)))
})

test_that("i=FALSE,j=TRUE drops all features but keeps imgData", {
    x <- ve[FALSE, TRUE]
    expect_true(nrow(x) == 0)
    expect_true(ncol(x) == ncol(ve))
    expect_identical(imgData(x), imgData(ve))
})

test_that("j=sample_id keeps only specified sample", {
    id <- sample(unique(ve$sample_id), 1)
    x <- ve[, ve$sample_id == id]
    expect_true(all(x$sample_id == id))
    expect_true(all(imgData(x)$sample_id == id))
})
