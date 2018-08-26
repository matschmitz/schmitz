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

#' @title Generate (scaled) noise mask from responses
#' @description Generate the noise mask (NM) over a given set of responses.
#' @param res Numerical vector with the reponses.
#' @param stim Numerical vector specifying the stimuli number.
#' @param N Matrix of noise pattern as generated with
#'   \code{\link[rcicr]{N <- generateStimuli2IFC(..., return_as_dataframe = TRUE)}}.
#' @param baseImg Numerical vector containing the baseImg image or string pointing to the baseImg image file.
#'   If baseImg is a string, then the baseImg image must in .png or .jpeg.
#' @param scaling String|Scalar|NULL specifying the scaling method. `"matched"` is the default method.
#'   If a scalar is provided (e.g. 5) than the `"constant"` method will be applied.
#'   If `NULL` no scaling is applied.
#' @return (Un)scaled Noise mask (NM).
#' @examples NULL
#' @export genNM
genNM <- function(res, stim, N, baseImg, scaling = "matched") {
    # Generate noise mask (NM)
    X  <- data.table::data.table(res = res, stim = stim)
    X  <- X[, .(res = mean(res)), stim]
    NM <- (N[, X$stim] %*% X$res) / length(X$res)

    # Read base image
    if (is.character(baseImg)) baseImg <- readBaseImg(baseImg)

    # Scale NM
    if (scaling == "matched") {
        scaledNM <- min(baseImg)+((max(baseImg)-min(baseImg))*(NM-min(NM))/(max(NM)-min(NM)))
    } else if (is.numeric(scaling)) { # constant scaling
        scaledNM <- (NM + scaling)/(2 * scaling)
        if (max(scaledNM) > 1 | min(scaledNM) < -1) warning("Constant is too low! Adjust.")
    } else if (is.null(scaling)) { # No scaling
        scaledNM <- NM
    }

    return(scaledNM)
}

#' @title Generate Classification Image (CI)
#' @description Generate the combinaed of the noise mask (NM) and the base image.
#' @inheritParams genNM
#' @param NM Noise mask as generated from \code{\link{genNM}}.
#' @param filename String specifying the name of the ouput CI. Default is `"combined.png"`.
#' @param outpath String specifying the output target path.
#' @param resize Scalar specifying if the image should be resized.
#' @param preview Logical indicating if the iamge should be previewed in the Viewer Panel.
#' @param ... List of parameters passed to \code{\link{genNM}}. If `NM` is provided, then ellipsis
#'   will be ignored.
#' @return NULL
#' @examples NULL
#' @export genCI
genCI <- function(NM = NULL, filename = "combined.png", outpath = "./cis",
                  resize = NULL, preview = FALSE, ...) {

    # Retrieve arguments passed
    dots <- list(...)

    # Read base image
    if (is.character(dots$baseImg)) baseImg <- readBaseImg(dots$baseImg)

    # Noise mask (NM)
    if (is.null(NM)) NM <- genNM(...)

    # Write and save combined image
    combined <- (baseImg + NM) / 2
    combined <- matrix(combined, nrow = 512)
    if (!dir.exists(outpath)) dir.create(outpath)
    imgPath <- file.path(outpath, filename)
    png::writePNG(combined, imgPath)

    # Resize
    if (!is.null(resize))  magick::image_read(imgPath) %>%
        magick::image_scale(resize) %>%
        magick::image_write(path = imgPath, format = "png")

    # Print image
    if (preview) print( magick::image_read(imgPath) )

    return(combined)
}

