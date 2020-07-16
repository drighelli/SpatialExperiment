#' getCellID
#' @description returns the column name where the cell identifiers are be stored
#' @param x a SpatialExperiment object instance
#' @aliases getCellID
#' @return the cellID
#' @export
#'
#' @examples
#' example(SpatialExperiment)
#' getCellID(se)
setMethod(f="getCellID", signature="SpatialExperiment", function(x)
{
    return(x@int_cellID)
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
#' example(SpatialExperiment)
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
#' @return none
#' @importFrom SingleCellExperiment int_colData int_colData<-
#' @importFrom methods is
#' @aliases spatialCoords<-
#' @export
#' @examples
#' example(SpatialExperiment)
#' fakeFishCoords <- cbind(fishCoordinates[,c(1:3)], fishCoordinates[,3])
#'         colnames(fakeFishCoords) <- c("MyCell_ID", "Irrelevant", "x", "y")
#' spatialCoords(se) <- fakeFishCoords
#' spatialCoords(se)
setReplaceMethod(f="spatialCoords", signature="SpatialExperiment", 
                function(x, value=DataFrame())
{
    if(is(value, "data.frame")) 
    {
        value <- DataFrame(value)
    }
    if(x@int_cellID != "rownames")
    {
        if(!(x@int_cellID %in% colnames(value)))
        {
            stop(paste0("Spatial coordinates haven't the defined cellColID. ",
                    "Expected: ", x@int_cellID))
        }
        dm <- dim(int_colData(x))
        ## Case of base SingleCellExperiment 
        ## (minimal dimensions are #ngenes x 2 with empty values)
        if(dm[2] == 2) 
        {
            int_colData(x) <- cbind(int_colData(x),value)
            x@int_spcIdx <- base::which(colnames(int_colData(x)) %in% 
                                            colnames(value))
        } else { ## case of already present spatial coordinates
            cDataIdx1 <- which(colnames(int_colData(x)) %in% colnames(value) )
            if(length(cDataIdx1) == 0)
            {
                stop("Spatial coordinates colnames differ from the stored ones")
            } else {
                cDataIdx <- match(value[[x@int_cellID]], 
                                int_colData(x)[[x@int_cellID]])
                for (col in colnames(value))
                {
                    colidx <- base::which(colnames(int_colData(x)) == col)
                    validx <- base::which(colnames(value) == col)
                    int_colData(x)[cDataIdx, colidx] <- value[,validx]
                }
            }
        }
    } else {
        stop("Please specify a different identifier")
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
#' example(SpatialExperiment)
#' spatialCoordsNames(se)
setMethod(f="spatialCoordsNames", signature="SpatialExperiment", function(x)
{
    return(colnames(int_colData(x)[x@int_spcIdx]))
})
