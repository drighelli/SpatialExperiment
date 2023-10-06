#' @rdname readMerscopeSPE
#' 
#' @title Load data from a Vizgen MERSCOPE experiment
#' 
#' @description
#' Creates a \code{\link{SpatialExperiment}} from the downloaded MERSCOPE   
#' directory for Vizgen MERSCOPE spatial gene expression data.
#'
#' @param dirname a directory path to MERSCOPE download that contains files of interest.
#' @param countmatfpattern a filename pattern for the count matrix. Default value is 
#' \code{"cell_by_gene.csv"}, and there is no need to change.
#' @param metadatafpattern a filename pattern for the metadata .csv file that 
#' contains spatial coords. Default value is \code{"metadata_file.csv"}, and 
#' there is no need to change.
#' @param coord_names a vector of two strings specify the spatial coord names. 
#' Default value is \code{c("center_x", "center_y")}, and there is no need to change.
#' 
#' @details
#' The constructor assumes the downloaded MERSCOPE count matrix and metadata in 
#' the same folder with the following structure:
#' MERSCOPE_folder/ \cr
#' · | — cell_by_gene.csv \cr
#' · | — cell_metadata.csv \cr
#'
#' @return  a \code{\link{SpatialExperiment}} object 
#' @export
#' 
#' @author Estella Yixing Dong
#'
#' @examples
#' \dontrun{
#' # Data download is from: 
#' https://console.cloud.google.com/storage/browser/vz-ffpe-showcase/
#' HumanOvarianCancerPatient2Slice2?pageState=(%22StorageObjectListTable%22:
#' (%22f%22:%22%255B%255D%22))&prefix=&forceOnObjectsSortingFiltering=false
#' 
#' # Here as an example, we have downsized the count matrix and meta data file to 
#' # only around 450 randomly selected cells and 20 genes, and necessary columns 
#' # of the metadata. 
#' 
#' merpath <- system.file(
#'   file.path("extdata", "VizgenMERSCOPE"),
#'   package = "SpatialExperiment")
#'   
#' list.files(merpath)
#' 
#' mer_spe <- readMerscopeSPE(dirname = merpath, 
#'                            countmatfpattern = "cell_by_gene.csv", 
#'                            metadatafpattern = "cell_metadata.csv", 
#'                            coord_names = c("center_x", "center_y"))
#' }
#' 
#' @importFrom SpatialExperiment SpatialExperiment
readMerscopeSPE <- function(dirname = dirname, 
                            countmatfpattern = "cell_by_gene.csv", 
                            metadatafpattern = "cell_metadata.csv", 
                            coord_names = c("center_x", "center_y")){
  
  countmat_file <- file.path(dirname, list.files(dirname, countmatfpattern))
  metadata_file <- file.path(dirname, list.files(dirname, metadatafpattern))
  
  # Read in 
  countmat <- read.csv(countmat_file)
  countmat <- countmat[order(countmat$cell), ]
  
  metadata <- read.csv(metadata_file)
  names(metadata)[names(metadata) == "X"] <- "cell"
  metadata <- metadata[order(metadata$cell), ]
  
  # Count matrix 
  rownames(countmat) <- countmat$cell + 1 # fix the indexing to start at 1
  counts <- t(subset(countmat, select = -cell))
  
  # rowData (does not exist)
  
  # colData
  rownames(metadata) <- metadata$cell + 1
  colData <- subset(metadata, select = -cell)
  
  
  spe <- SpatialExperiment::SpatialExperiment(
    assays = list(counts = counts),
    # rowData = rowData,
    colData = colData,
    spatialCoordsNames = coord_names
  )
  
  return(spe)
  
}
