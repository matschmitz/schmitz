#' @title Generate classification image
#' @description Generate the Classification Image (CI) over a given set of responses.
#' @param res Numerical vector with the reponses.
#' @param stim Numerical vector specifying the stimuli number.
#' @param N Matrix of noise pattern as generated with \code{\link[rcicr]{generateStimuli2IFC}}.
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
        scaledCI <- min(baseImg) + ((max(baseImg) - min(baseImg)) * (CI - min(CI)) / (max(CI) - min(CI)))
    } else if (is.numeric(scaling)) { # constant scaling
        scaledCI <- (CI + scaling)/(2 * scaling)
    } else { # No scaling
        scaledCI <- CI
    }
    return(scaledCI)
}


#' @title Generate combined image
#' @description Generate the combinaed image by imposing the selected noise on the base image.
#' @inheritParams scaleCI
#' @param outpath String specifying the output target path
#' @return NULL
#' @examples NULL
#' @export genCombinedImg
genCombinedImg <- function(CI, baseImg, filename = NULL, outpath = "./cis", size = 512) {
    if (is.null(filename)) filename <- "combined.jpeg"
    combined <- (baseImg + CI) / 2
    path <- file.path(outpath, filename)
    jpeg::writeJPEG(combined, path)
}

