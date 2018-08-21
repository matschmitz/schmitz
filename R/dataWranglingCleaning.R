#' @title Left Outter Join
#'
#' @description Update and merge columuns from `Y` onto `X`. If data tables are loaded, make sure
#'  to run `setDT(X)` and `setDT(Y)`.
#' @param X,Y a data.tables
#' @param onCol a string or character vector common to `X` and `Y` on which to merge the data
#' @return NULL
#' @examples NULL
#' @export loj
loj <- function(X = NULL, Y = NULL, onCol = NULL) {
    warning("If X or Y are loaded, make sure to previously setDT(X) and setDT(Y).")
    X[Y, (n) := mget(paste0("i.", n)), on = onCol]
}

#' @title Numerify
#'
#' @description Tries to convert each column of a data.table `X` to numeric (except logicals).
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
