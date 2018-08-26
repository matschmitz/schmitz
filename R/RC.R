#' @importFrom png writePNG readPNG
#' @importFrom magick image_read

#' @title Generate classification image
#' @description Generate the Classification Image (CI) over a given set of responses.
#' @param res Numerical vector with the reponses.
#' @param stim Numerical vector specifying the stimuli number.
#' @param N Matrix of noise pattern as generated with
#'   \code{\link[rcicr]{N <- generateStimuli2IFC(..., return_as_dataframe = TRUE)}}.
#' @return CI
#' @examples NULL
#' @export genCI
genCI <- function(res, stim, N) {
    X  <- data.table(res = res, stim = stim)
    X  <- X[, .(res = mean(res)), stim]
    CI <- (N[, X$stim] %*% X$res) / length(X$res)
}


#' @title Read and scale base image
#' @description Read and scale the base image
#' @param imgPath String specifying path to the base image.
#' @return Vector of pixels from the base image.
#' @examples NULL
#' @export readBaseImg
readBaseImg <- function(imgPath) {
    if (grepl('png|PNG', baseImg)) {
        baseImg <- png::readPNG(baseImg)
    } else {
        baseImg <- png::readJPEG(baseImg)
    }
    as.vector(baseImg)
}

#' @title Scale Classification Image
#' @description Scales the Cassification Image (CI).
#' @param CI Numerical vector of the CI, as generated with \code{\link{genCI}}.
#' @param baseImg Numerical vector containing the baseImg image or string pointing to the baseImg image file.
#'   If baseImg is a string, then the baseImg image must in .png or .jpeg.
#' @param scaling String|Scalar|NULL specifying the scaling method. `"matched"` is the default method.
#'   If a scalar is provided (e.g. 5) than the `"constant"` method will be applied.
#'   If `NULL` no scaling is applied.
#' @return Scaled CI.
#' @examples NULL
#' @export scaleCI
scaleCI <- function(CI, baseImg, scaling = "matched") {
    if (is.character(baseImg)) baseImg <- readBaseImg(baseImg)

    if (scaling == "matched") {
        scaledCI <- min(baseImg) + ((max(baseImg)-min(baseImg))*(CI-min(CI))/(max(CI)-min(CI)))
    } else if (is.numeric(scaling)) { # constant scaling
        scaledCI <- (CI + scaling)/(2 * scaling)
        if (max(scaledCI) > 1 | max(scaledCI) < -1) warning("Constant is too low! Adjust.")
    } else { # No scaling
        scaledCI <- CI
    }
    return(scaledCI)
}


#' @title Generate combined image
#' @description Generate the combinaed image by imposing the selected noise on the base image.
#' @inheritParams scaleCI
#' @param outpath String specifying the output target path.
#' @param resize Scalar specifying if the image should be resized.
#' @return NULL
#' @examples NULL
#' @export genCombinedImg
genCombinedImg <- function(CI, baseImg,
                           filename = NULL, outpath = "./cis",
                           resize = NULL, printImg = FALSE) {

    if (is.character(baseImg)) baseImg <- readBaseImg(baseImg)

    # Default filename
    if (is.null(filename)) filename <- "combined.png"

    # check dimensions
    if (dim(CI)[1] != 512) CI <- matrix(CI, nrow = 512)
    if (dim(baseImg)[1] != 512) CI <- matrix(baseImg, nrow = 512)

    # Write and save combined image
    combined <- (baseImg + CI) / 2
    if (!dir.exists(outpath)) dir.create(outpath)
    imgPath <- file.path(outpath, filename)
    png::writePNG(combined, imgPath)

    # Resize
    if (!is.null(resize))  magick::image_read(imgPath) %>%
        magick::image_scale(resize) %>%
        magick::image_write(path = imgPath, format = "png")

    # Print image
    if (printImg) print( magick::image_read(imgPath) )

    return(combined)
}

