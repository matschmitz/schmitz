#' @title Left Outter Join
#' @description Update and merge columuns from `Y` onto `X`. If data tables are loaded, make sure
#'  to run `setDT(X)` and `setDT(Y)`.
#' @param X,Y a data.tables
#' @param onCol a string or character vector common to `X` and `Y` on which to merge the data
#' @return NULL
#' @examples NULL
#' @export loj
loj <- function(X = NULL, Y = NULL, onCol = NULL) {
    warning("If X or Y are loaded, make sure to previously setDT(X) and setDT(Y).")
    n <- names(Y)
    X[Y, (n) := mget(paste0("i.", n)), on = onCol]
}

#' @title Numerify
#' @description Try to convert each column of a data.table `X` to numeric (except logicals).
#' @param X Data.table
#' @return NULL
#' @examples NULL
#' @export numerify
numerify <- function(X) {
    tryNumeric <- function(y) {
        if (!is.logical(y)) tryCatch(expr = {as.numeric(y)}, warning = function(cond) y) else y
    }
    X[, (names(X)) := lapply(.SD, tryNumeric)]
}

#' @title Extract number from string
#' @description Extracts a number from a string
#' @param x A string.
#' @return Extracted number.
#' @examples extractNumber("A1")
#' @importFrom stringr str_extract
#' @export extractNumber
extractNumber <- function(x, convertToNum = TRUE) {
    num <- stringr::str_extract(x, "\\-*\\d+\\.*\\d*")
    if(convertToNum) as.numeric(num)
}

#' @title Value Unmatching
#' @description Inverse of \code{\link[base]{match}}.
#' @param x A string.
#' @return Extracted number.
#' @examples 1 %ni% (1:3)
#' @export %ni%
"%ni%" <- Negate("%in%")
