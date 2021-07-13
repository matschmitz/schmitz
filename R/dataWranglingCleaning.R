#' @import data.table
#' @import magrittr

#' @title Left Outter Join
#' @description Update and merge columuns from `Y` onto `X`. If data tables are loaded, make sure
#'  to run `setDT(X)` and `setDT(Y)`.
#' @param X,Y a data.tables
#' @param onCol a string or character vector common to `X` and `Y` on which to merge the data
#' @return NULL
#' @examples NULL
#' @export loj
loj <- function(X = NULL, Y = NULL, onCol = NULL) {
    # data.table bug: https://github.com/Rdatatable/data.table/issues/1017
    if (truelength(X) == 0 | truelength(Y) == 0) stop("setDT(X) and setDT(Y) first")

    # apply loj
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
        if (!is.logical(y) | !is.factor(y)) {
            tryCatch(expr = {as.numeric(y)}, warning = function(w) y)
        } else {
            y
        }
    }
    X[, (names(X)) := lapply(.SD, tryNumeric)]
}

#' @title Roundify
#' @description Try to round each numerical column of a data.table and the prints it. Useful with
#'   rmarkdown reports.
#' @param X data.table
#' @param digits integer
#' @param printOut logical that indicates if the output needs to be printed
#' @return NULL
#' @examples NULL
#' @export roundify
roundify <- function(X, digits = 2) {
    Y <- copy(X) # avoids the original X being modified
    Y <- data.table(Y) # ensures Y is a data.table
    .cols <- names(which(sapply(Y, is.numeric)))
    Y[, (.cols) := lapply(.SD, round, digits = digits), .SDcols = .cols]
    Y
}

#' @title Round 2
#' @description Rounds the number to two decimal points.
#' If the value < .01 than a label "<.01" is used.
#' Removes leading zeros.
#' @param x a number or a numeric vector
#' @return a string or a character vector
#' @examples round2(c(0.033, 0.1, 0.0002))
#' @export round2
round2 <- function(x) gsub("0\\.", "\\.", ifelse(x < .01, "<.01", sprintf("%.2f", x)))

#' @title Round 3
#' @description Rounds the number to three decimal points.
#' If the value < .001 than a label "<.001" is used.
#' Removes leading zeros.
#' @param x a number or a numeric vector
#' @return a string or a character vector
#' @examples round3(c(0.033, 0.1, 0.0002))
#' @export round3
round3 <- function(x) gsub("0\\.", "\\.", ifelse(x < .001, "<.001", sprintf("%.3f", x)))

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

#' @title Supress output
#' @description Completely silence the output from a function
#' @param x a function with output
#' @return The function output
#' @export quiet
quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
}
