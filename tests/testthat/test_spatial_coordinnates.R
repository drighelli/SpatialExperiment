context("SpatialExperiment object")
test_that("spatial coordinates methods are functioninng", {
    fishCoordinates <- data.frame(Cell_ID=paste0("cell",c(1:30)), 
                     Irrelevant=100, 
                     x=sample(c(-4000:4000), size=30, replace=TRUE),
                     y=sample(c(-4000:4000), size=30, replace=TRUE))
    
    fishCellLabels <- data.frame(ID=paste0("cell",c(1:30)), 
                                class="neuron", 
                                classID=sample(c(0:5), size=30, replace=TRUE))
    
    fishCounts <- matrix(sample(0:100, size=(30*30), replace=TRUE),
                         nrow=30, ncol=30,
                         dimnames=list(paste0("gene",c(1:30)), 
                                    paste0("cell",c(1:30))))
    
    se <- SpatialExperiment(rowData=rownames(fishCounts),
                            colData=fishCellLabels,
                            assays=SimpleList(counts=as.matrix(fishCounts)),
                            spatialCoords=fishCoordinates)
    expect_s4_class(object=SpatialExperiment(assays=SimpleList(
                        counts=as.matrix(fishCounts))), 
                    class="SpatialExperiment")
    ## SpatialExperiment automatically loads data in DataFrame format
    ## expect_false(is.data.frame(spatialCoords(se)))
    expect_error(expect_identical(fishCoordinates, spatialCoords(se)))
    expect_identical(DataFrame(fishCoordinates), spatialCoords(se))
})

