#' @title Schmitz Package
#'
#' @description Left outter join on X
#' @param X a data.table
#' @param Y a data.table
#' @param onCol a string or vector common to X and Y on which to merge the data
#' @return NULL
#' @examples NULL
#' @export loj
loj <- function(X = NULL, Y = NULL, onCol = NULL) {
    giveExemple <- is.null(X) & is.null(Y) & is.null(onCol)
    if (giveExemple) {
        cat("\nExemple:")
        cat("\n>X\n")
        X <- data.table(id = 1:5, L = letters[1:5]) %T>% print
        cat("\n>Y\n")
        Y <- data.table(id = 3:5, L = c(NA, "g", "h"), N = c(10, NA, 12)) %T>% print
        onCol <- "id"
        cat('\nLOJ(X, Y, "id")\n')
    }
    n <- names(Y)
    X[Y, (n) := mget(paste0("i.", n)), on = onCol]
    if (giveExemple) {cat(">X\n"); print(X)}
}
