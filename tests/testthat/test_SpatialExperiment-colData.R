example(read10xVisium, echo = FALSE)

test_that("colData()<-NULL throws warning", {
    expect_warning(colData(spe) <- NULL)
})

test_that("x$sample_id<- needs one-to-one mapping", {
    expect_warning(spe$sample_id <- NULL)
    expect_warning(spe$sample_id <- sample(c(1, 2), ncol(spe), TRUE))
})

test_that("valid x$sample_id replacement updates imgData", {
    old <- unique(spe$sample_id)
    new <- paste0(old, "x")
    i <- match(spe$sample_id, old)
    tmp <- spe; tmp$sample_id <- new[i]
    for (i in seq_along(old))
        expect_equivalent(
            spe[, spe$sample_id == old[i]],
            tmp[, tmp$sample_id == new[i]])
    i <- match(imgData(spe)$sample_id, old)
    expect_identical(imgData(tmp)$sample_id, new[i])
})
