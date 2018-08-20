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
    X[Y, (n) := mget(paste0("i.", n)), on = onCol]
    if (giveExemple) {cat(">X\n"); print(X)}
}
