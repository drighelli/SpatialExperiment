#' @rdname readImgData
#' @title Read images & scale factors 
#' 
#' @param path ...
#' @param sampleId ...
#' @param images ...
#' @param scaleFactors ...
#' @param load logical; should the image(s) be loaded into memory
#'   as a \code{grob}? If FALSE, will store the path/URL instead.
#' 
#' @return a \code{\link{DataFrame}}
#' 
#' @author Helena L. Crowell
#' 
#' @examples 
#' dir <- system.file(
#'   file.path("extdata", "10xVisium", "spatial"),
#'   package = "SpatialExperiment")
#'   
#' # base directory contains 
#' # - scale factors (scalefactors_json.json)
#' # - one image (tissue_lowres_image.png)
#' list.files(dir)
#' 
#' # read in images & scale factors
#' # as valid 'imgData' 'DFrame'
#' readImgData(dir, sampleId = "foo")
#' 
#' @importFrom rjson fromJSON
#' @importFrom grid rasterGrob unit
#' @importFrom magick image_read image_info
#' @export

# TODO: change 'as' to 'load = TRUE/FALSE' &
# support images to be supplied as path or URL

readImgData <- function(path=".", sample_id=names(path), 
    imageSources=file.path(path, "tissue_lowres_image.png"), 
    scaleFactors=file.path(path, "scalefactors_json.json"),
    load=TRUE)
{
    # get sample identifiers
    if (is.null(sample_id))
        stop("'sample_id' mustn't be NULL")
    stopifnot(
        is.character(sample_id),
        length(unique(sample_id)) == length(path))
    names(path) <- names(scaleFactors) <- sample_id
    
    # put images into list with one element per sample
    images <- lapply(path, function(.) grep(., imageSources, value=TRUE))
    
    dfs <- lapply(sample_id, function(sid) 
    {
        sfs <- fromJSON(file=scaleFactors[sid])
        dfs <- lapply(images[[sid]], function(img) 
        {
            # get image identifier
            img_nm <- gsub("\\..*", "", basename(img))
            iid <- switch(img_nm,
                tissue_lowres_image="lowres",
                tissue_hires_image="hires",
                detected_tissue_image="detected",
                aligned_fiducials="aligned")
            # get scale factor
            sf_nm <- switch(iid,
                lowres="tissue_lowres_scalef",
                "tissue_hires_scalef")
            sf <- sfs[[grep(sf_nm, names(sfs))]]
            # get 'imgData'
            .get_imgData(img, sf, sid, iid, load)
        })
        do.call(rbind, dfs)
    })
    do.call(rbind, dfs)
}
