#' @importFrom png writePNG readPNG
#' @importFrom jpeg readJPEG
#' @importFrom magick image_read

#' @title Read and scale base image
#' @description Read and scale the base image
#' @param baseImgPath String specifying path to the base image.
#' @return Vector of pixels from the base image.
#' @examples NULL
#' @export readBaseImg
readBaseImg <- function(baseImgPath, maxContrast = TRUE) {
    # Read image
    if (grepl('png|PNG', baseImgPath)) {
        baseImg <- png::readPNG(baseImgPath)
    } else {
        baseImg <- jpeg::readJPEG(baseImgPath)
    }

    # Ensure there is only 2 dimensions
    if (length(dim(baseImg)) == 3) baseImg <- baseImg[, , 1]

    # Maximize base image contrast
    if (maxContrast) baseImg <- (baseImg - min(baseImg))/(max(baseImg) - min(baseImg))

    return( as.vector(baseImg) )
}

#' @title Generate (and scale) mask from responses
#' @description Generate (and scale) mask from responses.
#' @param response Numerical vector with the reponses.
#' @param stim Numerical vector specifying the stimuli number.
#' @param noiseMatrix Matrix of noise pattern as generated with
#'   \code{\link[rcicr]{noiseMatrix <- generateStimuli2IFC(..., return_as_dataframe = TRUE)}}.
#' @param baseImg Numerical vector containing the baseImg image or string pointing to the baseImg
#'   image file. If baseImg is a string, then the baseImg image must in .png or .jpeg.
#' @param scaling String|Scalar|NULL specifying the scaling method. `"matched"` is the default method.
#'   If a scalar is provided (e.g. 5) than the `"constant"` method will be applied.
#'   If `NULL` no scaling is applied.
#' @return (Un)scaled Noise mask (mask).
#' @examples NULL
#' @export genMask
genMask <- function(response, stim, noiseMatrix, baseImg, scaling = "matched") {
    # Generate mask
    X  <- data.table::data.table(response = response, stim = stim)
    X  <- X[, .(response = mean(response)), stim]
    mask <- (noiseMatrix[, X$stim] %*% X$response) / length(X$response)

    # Read base image
    if (is.character(baseImg)) baseImg <- readBaseImg(baseImg)

    # Scale mask
    if (scaling == "matched") {
        scaledMask <- min(baseImg)+((max(baseImg)-min(baseImg))*(mask-min(mask))/(max(mask)-min(mask)))
    } else if (is.numeric(scaling)) { # constant scaling
        scaledMask <- (mask + scaling)/(2 * scaling)
        if (max(scaledMask) > 1 | min(scaledMask) < -1) warning("Constant is too low! Adjust.")
    } else if (is.null(scaling)) { # No scaling
        scaledMask <- mask
    }

    return(scaledMask)
}

#' @title Generate Classification Image (CI)
#' @description Generate the combinaed of the noise mask (mask) and the base image.
#' @inheritParams genMask
#' @param mask Mask as generated from \code{\link{genMask}}. If `NULL` (default), then the `mask`
#'   will be generated automatically by calling \code{\link{genMask}}. See `...`.
#' @param filename String specifying the name of the ouput CI. Default is `"combined.png"`.
#' @param outpath String specifying the output target path.
#' @param resize Scalar specifying if the image should be resized.
#' @param preview Logical indicating if the iamge should be previewed in the Viewer Panel.
#' @param ... List of parameters passed to \code{\link{genMask}}. If `mask` is provided, then ellipsis
#'   will be ignored.
#' @return NULL
#' @examples NULL
#' @export genCI
genCI <- function(mask = NULL, filename = "combined.png", outpath = "./cis",
                  resize = NULL, preview = FALSE, ...) {

    # Retrieve arguments passed
    dots <- list(...)

    # Read base image
    if (is.character(dots$baseImg)) dots$baseImg <- readBaseImg(dots$baseImg)

    # Generate mask
    if (is.null(mask)) mask <- genMask(...)

    # Write and save combined image
    baseImg <- dots$baseImg
    combined <- (baseImg + mask) / 2
    combined <- matrix(combined, nrow = 512)
    if (!dir.exists(outpath)) dir.create(outpath)
    imgPath <- file.path(outpath, filename)
    png::writePNG(combined, imgPath)

    # Resize
    if (!is.null(resize))  magick::image_read(imgPath) %>%
        magick::image_scale(resize) %>%
        magick::image_write(path = imgPath, format = "png")

    # Preview image
    if (preview) print( magick::image_read(imgPath) )
}

