# LIBRARIES ----------------------------------------------------------------------------------------
#' @importFrom png writePNG readPNG
#' @importFrom jpeg readJPEG
#' @importFrom magick image_read
#' @importFrom imager width height Xc Yc

# DATA ---------------------------------------------------------------------------------------------
#' @title RC data
#' @description Fake RC data
#' @format A data frame with 80 rows and 4 variables:
#' \describe{
#'   \item{id}{Participant's unique identifier.}
#'   \item{stimuli}{Stimuli number. This is pared with the noiseMatrix (\code{N}), e.g. stimuli 1
#'     was generated from the base image and the first column of N (\code{N[, 1]}).}
#'   \item{reponses}{Responses from participant. Can be \code{1} (oriented image selected) or
#'     \code{-1} (inverted image selected).}
#'   \item{condition}{Two conditions: \code{A} and \code{B}.}
#' }
#' @usage data(RC)
"RC"

#' @title noiseMatrix data
#' @description Fake noiseMatrix data
#' @format A (512^2)x20 matrix of noise as generated with
#'   \code{N <- matrix(rcicr::generateCI2IFC(..., return_as_dataframe = TRUE))}.
#' @usage data(N)
"N"

# FUNCTIONS ----------------------------------------------------------------------------------------
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
#' @param preview Logical indicating if the image should be previewed in the Viewer Panel.
#' @param ... List of parameters passed to \code{\link{genMask}}. If `mask` is provided, then ellipsis
#'   will be ignored.
#' @return NULL
#' @examples NULL
#' @export genCI
genCI <- function(mask = NULL, filename = "combined.png", outpath = "./cis/",
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
    if (!is.null(resize)) magick::image_read(imgPath) %>%
        magick::image_scale(resize) %>%
        magick::image_write(path = imgPath, format = "png")

    # Preview image
    if (preview) invisible(capture.output(print(magick::image_read(imgPath))))

    invisible(capture.output(imgPath))
}


#' @title Get face region
#' @description Returns a logical vector with the face region
#' @param imgPath String specifying path to the base image.
#' @param xpos Numeric specifiying the X position (relative to the center).
#' @param ypos Numeric specifiying the Y position (relative to the center).
#' @param faceWidth Numeric specifiying the width of the face region.
#' @param faceHeight Numeric specifiying the height of the face region.
#' @param preview Numeric specifiying the height of the face region.
#' @param writeImgTo String specifying where and if the output image should be saved. Default is
#'   NULL, meaning that the image will not be saved.
#' @return Logical vector specifying the location of the face region
#' @examples NULL
#' @export getFaceRegion
getFaceRegion <- function(imgPath,
                          xpos = 0, ypos = 0, faceWidth = 1.4, faceHeight = 1.8,
                          preview = TRUE, writeImgTo = NULL) {
    # Read image and convert to matrix
    face <- readBaseImg(imgPath)
    faceLength <- sqrt(length(face)) # must be squared
    face <- matrix(face, ncol = faceLength)

    # Define face region: https://dahtah.github.io/imager/gimptools.html
    Xcc <- function(im) imager::Xc(im) - imager::width(im)/2  + ypos
    Ycc <- function(im) imager::Yc(im) - imager::height(im)/2 + xpos
    NN <- imager::as.cimg( matrix(1:faceLength^2, nrow = faceLength) )
    faceRegion <- (Xcc(NN)/faceHeight)^2 + (Ycc(NN)/faceWidth)^2 < 100^2
    faceRegion <- as.vector(faceRegion)

    # Preview in Viewer
    if (preview) {
        alphaMask <- matrix(1, faceLength, faceLength)
        alphaMask[!faceRegion] <- 0.6
        previewFace <- abind::abind(face, alphaMask, along = 3)
        previewFacePath <- tempfile(fileext = ".png")
        png::writePNG(previewFace, previewFacePath)
        invisible(capture.output(print(magick::image_read(previewFacePath))))
    }
    # Write face
    if (!is.null(writeImgTo)) {
        alphaMask <- matrix(1, faceLength, faceLength)
        alphaMask[!faceRegion] <- 0
        printedFace <- abind::abind(face, alphaMask, along = 3)
        png::writePNG(printedFace, writeImgTo)
    }

    invisible(faceRegion)
}
