#' checkSpatialCoords
#' @description checks if all the spatial parameters have the right fields.
#' @param SpatialExperiment a SpatialExperiment class object
#' @importFrom SingleCellExperiment colData int_colData int_colData<-
#' @importFrom S4Vectors DataFrame
#' @importFrom methods is as 
#' @return the SpatialExperiment class object passed as input.
#' @keywords internal
setMethod(f="checkSpatialCoords",
          signature="SpatialExperiment",
          definition=function(se, spatialCoords=DataFrame())
{
    stopifnot(is(se, "SpatialExperiment"))
    stopifnot( (sum( c("Barcodes", "ID") %in% colnames(colData(se))) != 0)) 
    
    if(class(spatialCoords) == "data.frame") 
    {
        spatialCoords <- as(spatialCoords, "DataFrame")
    }
    if("ID" %in% colnames(colData(se)))
        cDataIdx <- match(colData(se)$ID, spatialCoords$ID)
    else
        cDataIdx <- match(colData(se)$Barcodes, spatialCoords$Barcodes)
    int_colData(se) <- as(cbind(spatialCoords[cDataIdx,], 
                                           int_colData(se)), 
                            "DataFrame")
    se@int_spcIdx <- base::which(colnames(int_colData(se)) %in% 
                                colnames(spatialCoords))
    return(se)
})


#' spatialCoords-getter
#' @description a getter method which returns the spatial coordinates previously
#' stored in a SpatialExperiment class object.
#' @param x A SpatialExperiment class object.
#'
#' @return a DataFrame within the spatial coordinates.
#' 
#' @export
#' @aliases spatialCoords
#' @examples
#' spatialDataFile <- system.file(file.path("extdata", "seqFISH",
#'     "seqFISH.RData"), package="SpatialExperiment")
#' load(spatialDataFile)
#' se <- SpatialExperiment(rowData=rownames(fishFeaturesCounts),
#'     colData=fishCellLabels,
#'     assays=SimpleList(counts=as.matrix(fishFeaturesCounts)),
#'     spatialCoords=fishCoordinates)
#' spatialCoords(se)
setMethod(f="spatialCoords", signature="SpatialExperiment", function(x)
{
    return(int_colData(x)[, x@int_spcIdx])
})

#' spatialCoords-setter
#' @description a setter method which sets/replaces the spatial coordinate in a 
#' SpatialExperiment class object.
#' @param x a SpatialExperiment class object
#' @param value a DataFrame with the new spatial coordinates to set.
#'
#' @export
#' @aliases spatialCoords<-
#' @examples
#' spatialDataFile <- system.file(file.path("extdata", "seqFISH",
#'     "seqFISH.RData"), package="SpatialExperiment")
#' load(spatialDataFile)
#' se <- SpatialExperiment(rowData=rownames(fishFeaturesCounts),
#'     colData=fishCellLabels,
#'     assays=SimpleList(counts=as.matrix(fishFeaturesCounts)),
#'     spatialCoords=fishCoordinates)
#' fakeFishCoords <- cbind(fishCoordinates[,c(1:3)], fishCoordinates[,3])
#'         colnames(fakeFishCoords) <- c("ID", "Irrelevant", "x", "y")
#' spatialCoords(se) <- fakeFishCoords
#' spatialCoords(se)
setReplaceMethod(f="spatialCoords", signature="SpatialExperiment", 
                function(x, value)
{
    stopifnot(("ID" %in% colnames(value)),
                ("Irrelevant" %in% colnames(value)),
                ("x" %in% colnames(value)),
                ("y" %in% colnames(value)))
    cDataIdx <- match(value$ID, int_colData(x)$ID)
    
    for (col in colnames(value))
    {
        colidx <- base::which(colnames(int_colData(x)) == col)
        validx <- base::which(colnames(value) == col)
        int_colData(x)[cDataIdx, colidx] <- value[,validx]
    }
    
    return(x)
})


#' spatialCoordsNames-getter
#' @description getter method for the spatial coordinates names in a 
#' SpatialExperiment class object.
#' @param x a SpatialExperiment class object.
#'
#' @return a vector with the colnames of the spatial coordinates.
#' @export
#' @aliases spatialCoordsNames
#' @examples
#' spatialDataFile <- system.file(file.path("extdata", "seqFISH",
#'     "seqFISH.RData"), package="SpatialExperiment")
#' load(spatialDataFile)
#' se <- SpatialExperiment(rowData=rownames(fishFeaturesCounts),
#'     colData=fishCellLabels,
#'     assays=SimpleList(counts=as.matrix(fishFeaturesCounts)),
#'     spatialCoords=fishCoordinates)
#' spatialCoordsNames(se)
setMethod(f="spatialCoordsNames", signature="SpatialExperiment", function(x)
{
    return(colnames(int_colData(x)[x@int_spcIdx]))
})
