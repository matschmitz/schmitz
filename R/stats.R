#' @import data.table
#' @import magrittr
#' @importFrom lmSupport modelEffectSizes

#' @title Formated mean and standard deviation
#'
#' @description Format the mean and sd of a vector
#' @param x a numerical vector
#' @return formated string
#' @examples m_sd(1:3)
#' @export m_sd
m_sd <- function(x, ...) sprintf("%.2f(%.2f)", mean(x, ...), sd(x, ...))

#' @title Effect sizes for lm summary
#'
#' @description Add effect sizes to the lm summary
#' @param mdl an lm model obtained from lm
#' @param digits number of decimal digits
#' @return data table with summary statistics
#' @examples ss(mdl)
#' @export ss
ss <- function(mdl, digits = 3) {
    mdl.coefficients <- summary(mdl)$coefficients
    mdl.effects <- lmSupport::modelEffectSizes(mdl, Print = FALSE)$Effects[, 2:3]
    mdl.s <- data.table(cbind(mdl.coefficients, mdl.effects))
    mdl.s <- schmitz::roundify(mdl.s, digits)
    return(mdl.s)
}
