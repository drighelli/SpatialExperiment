example(read10xVisium, echo = FALSE)

test_that("duplicated sample_ids are made unique with a message", {
    expect_message(new <- cbind(spe, spe))
    expect_equal(
        length(unique(new$sample_id)), 
        2*length(unique(spe$sample_id)))
    expect_true(nrow(new) == nrow(spe))
    expect_true(ncol(new) == 2*ncol(spe))
    expect_identical(rownames(new), rownames(spe))
    expect_setequal(colnames(new), colnames(spe))
})

test_that("imgData are combined correctly", {
    spe1 <- spe; spe2 <- spe
    spe1$sample_id <- paste(spe1$sample_id, "A", sep=".")
    spe2$sample_id <- paste(spe2$sample_id, "B", sep=".")
    expect_silent(spe3 <- cbind(spe1, spe2))
    # imgData are kept only once
    expect_true(sum(grepl("imgData", names(int_metadata(spe3)))) == 1)
    # number of image entries should be doubled
    expect_equal(nrow(imgData(spe3)), 2*nrow(imgData(spe)))
    # sample_ids should be combined from input SPEs
    expect_setequal(
        unique(spe3$sample_id), 
        imgData(spe3)$sample_id)
    expect_setequal(
        unique(spe3$sample_id), 
        unique(c(spe1$sample_id, spe2$sample_id)))
    # imgData from each input SPE should be the same
    one <- seq(nrow(imgData(spe1)))
    two <- max(one)+seq(nrow(imgData(spe2)))
    expect_identical(imgData(spe3)[one, ], imgData(spe1))
    expect_identical(imgData(spe3)[two, ], imgData(spe2))
})
