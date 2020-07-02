#' checkSpatialCoords
#' @description checks if all the spatial parameters have the right fields.
#' @param se a SpatialExperiment class object
#' @param spatialCoords a DataFrame with spatial coordinates (x,y,ID/Barcodes)
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
    if(sum(dim(spatialCoords) == c(0,0)) != 2)
    {
        stopifnot( (sum( c("Barcodes", "ID") %in% colnames(colData(se))) != 0)) 
    }
    
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
                function(x, value=DataFrame())
{
    if(is(value, "data.frame")) 
    {
        value <- DataFrame(value)
    }
                    
    dm <- dim(int_colData(x))
    ## Case of base SingleCellExperiment 
    ## (minimal dimensions are #ngenes x 3 with empty values)
    if(dm[2] == 3) 
    {
        int_colData(x) <- cbind(int_colData(x),value)
        x@int_spcIdx <- base::which(colnames(int_colData(x)) %in% 
                                        colnames(value))
    } else { ## case of already present spatial coordinates
        
        cDataIdx1 <- which(colnames(value) %in% colnames(int_colData(x)))
        if(length(cDataIdx1) == 0)
        {
            stop("Spatial coordinates colnames differs from the stored ones!")
        } else {
            stopifnot(("ID" %in% colnames(value)))
            cDataIdx <- match(value$ID, int_colData(x)$ID)
            for (col in colnames(value))
            {
                colidx <- base::which(colnames(int_colData(x)) == col)
                validx <- base::which(colnames(value) == col)
                int_colData(x)[cDataIdx, colidx] <- value[,validx]
            }
        }
    }  
        # for (col in colnames(value))
        # {
        #     colidx <- base::which(colnames(int_colData(x)) == col)
        #     validx <- base::which(colnames(value) == col)
        #     int_colData(x)[cDataIdx, colidx] <- value[,validx]
        # }
    # }
    # stopifnot(("ID" %in% colnames(value)),
    #             ("Irrelevant" %in% colnames(value)),
    #             ("x" %in% colnames(value)),
    #             ("y" %in% colnames(value)))
    # cDataIdx <- match(value$ID, int_colData(x)$ID)
    # 
    # for (col in colnames(value))
    # {
    #     colidx <- base::which(colnames(int_colData(x)) == col)
    #     validx <- base::which(colnames(value) == col)
    #     int_colData(x)[cDataIdx, colidx] <- value[,validx]
    # }
    
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
